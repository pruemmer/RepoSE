
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import nd._

import scala.{Option => SOption}
import scala.collection.JavaConverters._
import scala.collection.mutable.{HashMap => MHashMap, ArrayBuffer}
import scala.collection.{Map => GMap}

object Constants {

  val FillVarName        = """([0-9]+) Fill ([0-9]+)""".r
  val MatchFlagName      = """IsMatch_/(.+)/_([0-9]+)""".r

  val fillVarName        = " Fill "
  val EncodingMarkerName = """EncodingMarker_[0-9]+""".r

  import ASTMatchers._
  def markerAssertion(num : String) =
    AssertCmd(PlainApp("EncodingMarker_" + num))

}

object MatchRecoder extends BacktrackingSearch {
  import ASTMatchers._
  import Constants._

  def apply(cmds : Seq[Command]) : Seq[Command] = {
    var num     = 0
    var curCmds = cmds

    var cont    = true
    while (cont)
      findOccurrence(curCmds) match {
        case Some(occ) => {
          curCmds = recode(curCmds, occ, num)
          num = num + 1
        }
        case None =>
          cont = false
      }

    curCmds
  }

  val printer = new PrettyPrinterNonStatic

  case class MatchOcc(startInd         : Int,
                      membershipInd    : Int,
                      concatInd        : Int,
                      matchInd         : Int,
                      regex            : String,
                      stringVar        : Term,
                      otherStringTerms : Seq[Term],
                      cgVars           : Seq[String],
                      fillNameIndex    : String)

  def recode(cmds : Seq[Command], occ : MatchOcc, num : Int) : Seq[Command] = {
    import occ._

    val numCG = Reg2SMT.numCaptureGroups(regex)

    println("Rewriting match: " + occ)
    println("  Equation: " + (printer print cmds(concatInd)))
    println("  Extracted variables: " + (cgVars mkString ", "))
    println("  Capture groups found: " + numCG)

    var newCmds = cmds

    val preRewritings, rewritings = new MHashMap[Term, Term]
    val preRewriter = new TermReplacer(preRewritings)

    // sometimes multiple representations of the processed string variable occur,
    // first unify those
    for (t <- otherStringTerms)
      preRewritings.put(t, stringVar)

    Options.matchEncoding match {
      case Options.MatchEncoding.PrioTransducer => {
        val ((transducerFuns, transducerDefs), doReplace, cgVarsToAssign) =
          (numCG, cgVars.size) match {
            case (0, n) if n >= 2 =>
              (Reg2PT("(.*?)" + regex + "(.*)", "MatchTD_" + num + "_"),
               true, List(cgVars.head, cgVars.last))
            case (n1, n2) if n1 == n2 =>
              (Reg2PT(regex, "MatchTD_" + num + "_"),
               false, cgVars)
            case (n1, n2) if n2 == n1 + 2 =>
              (Reg2PT("(.*?)" + regex + "(.*)", "MatchTD_" + num + "_"),
               true, cgVars)
            case n =>
              throw new Exception(
                "Number of variables and capture groups is inconsistent")
          }

        val transducerApps =
          for ((cgVar, tdFun) <- cgVarsToAssign zip transducerFuns)
          yield AssertCmd(PlainApp(tdFun, stringVar, PlainApp(cgVar)))

        var remCmds = newCmds drop matchInd
        remCmds = remCmds map (_.accept(preRewriter, ()))
        if (doReplace) {
          val visitor1 =
            new SubstringReplacer(cgVars.head, stringVar, cgVars.last)
          remCmds = remCmds map (_.accept(visitor1, ()))

          val visitor2 =
            new ResultReplacer(cgVars.head, cgVars.last, cgVars,
                               "replace_" + fillNameIndex)
          remCmds = remCmds map (_.accept(visitor2, ()))

          for ((t, v) <- visitor2.replacedTerms.toSeq.sortBy(_._2)) {
            remCmds =
              Parsing.parseString("(declare-const " + v + " String)") ++
              List(AssertCmd(PlainApp("=", PlainApp(v), t))) ++
              remCmds
          }

          rewritings ++= extractLengthSubst(visitor2.replacedTerms)
        }

        newCmds =
          (newCmds take startInd) ++
          transducerApps ++
          List(markerAssertion(fillNameIndex),
               newCmds(membershipInd).accept(preRewriter, ())) ++
          remCmds

        newCmds = Transducers.addTransducers(newCmds, transducerDefs)
      }

      case Options.MatchEncoding.RegexTerm => {
        val (regexTerm, doReplace, cgVarsToAssign) =
          (numCG, cgVars.size) match {
            case (0, n) if n >= 2 =>
              (Reg2SMT("(.*)" + regex + "(.*)"),
               true, List(cgVars.head, cgVars.last))
            case (n1, n2) if n1 == n2 =>
              (Reg2SMT(regex),
               false, cgVars)
            case (n1, n2) if n2 == n1 + 2 =>
              (Reg2SMT("(.*)" + regex + "(.*)"),
               true, cgVars)
            case n =>
              throw new Exception(
                "Number of variables and capture groups is inconsistent")
          }

        val regexVar = "re!1"
        val extractors =
          for ((cgVar, n) <- cgVarsToAssign.zipWithIndex)
          yield PlainApp("=",
                         PlainApp(cgVar),
                         FunApp(IndexedSymbol("str.extract", (n+1).toString),
                                stringVar, PlainApp(regexVar)))
        val extractorConj =
          AssertCmd(Let(PlainApp("and", extractors : _*),(regexVar, regexTerm)))

/*
        newCmds = newCmds.patch(concatInd, List(extractorConj), 1)
        newCmds = newCmds.patch(startInd, List(markerAssertion(fillNameIndex)),
                                membershipInd - startInd)
 */

        var remCmds = newCmds drop matchInd
        remCmds = remCmds map (_.accept(preRewriter, ()))
        if (doReplace) {
          val visitor1 =
            new SubstringReplacer(cgVars.head, stringVar, cgVars.last)
          remCmds = remCmds map (_.accept(visitor1, ()))

          val visitor2 =
            new ResultReplacer(cgVars.head, cgVars.last, cgVars,
                               "replace_" + fillNameIndex)
          remCmds = remCmds map (_.accept(visitor2, ()))

          val smallRegexTerm = Reg2SMT(regex)
          for ((t, v) <- visitor2.replacedTerms.toSeq.sortBy(_._2)) {
            val declaration =
              Parsing.parseString("(declare-const " + v + " String)")

            val addCmds =
            translateReplacementTerm(t, cgVars) match {
              case PlainApp("str.to.re", r@StringLit(_)) =>
                declaration ++
                List(AssertCmd(PlainApp("=",
                                        PlainApp(v),
                                        PlainApp("str.replace_re",
                                                 stringVar,
                                                 smallRegexTerm,
                                                 r))))
              case replTerm =>
                declaration ++
                List(AssertCmd(PlainApp("=",
                                        PlainApp(v),
                                        PlainApp("str.replace_cg",
                                                 stringVar,
                                                 smallRegexTerm,
                                                 replTerm))))
            }

            remCmds = addCmds ++ remCmds
          }

          rewritings ++= extractLengthSubst(visitor2.replacedTerms)
        }

        newCmds =
          (newCmds take startInd) ++
          List(extractorConj,
               markerAssertion(fillNameIndex),
               newCmds(membershipInd).accept(preRewriter, ())) ++
          remCmds
      }
    }

    val visitor = new TermReplacer (rewritings)
    newCmds map (_.accept(visitor, ()))
  }

  //////////////////////////////////////////////////////////////////////////////

  def translateReplacementTerm(t : Term, cgVars : Seq[String]) : Term = {
    def toRE(s : Term) : Term = s match {
      case x@StringLit(_) => PlainApp("str.to.re", x)
      case PlainApp(v)    => FunApp(IndexedSymbol("re.reference",
                                                  "" + (cgVars indexOf v)))
    }

    val PlainApp("str.++", args @ _*) = t

    args.drop(1).dropRight(1) match {
      case Seq()   => PlainApp("str.to.re", StringLit(""))
      case Seq(x)  => toRE(x)
      case midArgs => PlainApp("re.++", midArgs map toRE : _*)
    }
  }

  //////////////////////////////////////////////////////////////////////////////

  def findOccurrence(cmds : Seq[Command]) : SOption[MatchOcc] =
    search[MatchOcc] {
      chooseInt(0 until cmds.size) { start =>
        val startCmd = cmds(start)
        assume(startCmd.isInstanceOf[AssertCommand])

        // constraints use the |n Fill m| variables
        val FillNameIndex = assumeIsDefined {
          FindSymbolVisitor(startCmd) {
            case FillVarName(num1, "0") => Some(num1)
            case _                      => None
          }
        }

        val CGEquation = new CaptureGroupEquation(FillNameIndex)

        // the |n Fill m| variables with this index do not
        // occur before the considered constraints
        assumeForall(0 until start) { ind =>
          !(cmds(ind).isInstanceOf[AssertCommand] &&
              ContainsSymbolVisitor(cmds(ind)) {
                case FillVarName(FillNameIndex, _) => true
                case _ => false
              })
        }

        // identify the |IsMatch_regex| flag
        val (matchInd, matchFlagNum, regex) =
          findMinInt[(Int, String, String)]((start + 1) until cmds.size) { end =>
            val (regex, num) = assumeIsDefined {
              cmds(end) match {
                case AssertCmd(PlainApp(MatchFlagName(regex, num))) =>
                  Some((regex, num))
                case _ =>
                  None
              }
            }
            success((end, num, regex))
          }

        def matchOcc(cmd : Command) =
          ContainsSymbolVisitor(cmd) {
            case MatchFlagName(`regex`, `matchFlagNum`) => true
            case _ => false
          }

        alternatives(2) {
          case 0 => {
            // there is only one occurrence of the |IsMatch_regex| flag
            assumeForall(start until matchInd) {
              ind => !matchOcc(cmds(ind))
            }

            // there is a regular membership query defining the string variable
            val (regexInd, mainVar) =
              findMaxInt[(Int, Term)](start until matchInd) { ind =>
                val mainVar = assumeIsDefined {
                  cmds(ind) match {
                    case AssertCmd(PlainApp("str.in.re",
                                            mainVar, _)) =>
                      Some(mainVar)
                    case _ =>
                      None
                  }
                }
                success((ind, mainVar))
              }

            // there is an equation in which the main variable is decomposed
            // into multiple string variables, corresponding to the capture
            // groups (and other parts of the regex)
            alternatives(2) {
              case 0 =>
                chooseInt(start until matchInd) { concatInd =>
                  val groups = assumeIsDefined {
                    cmds(concatInd) match {
                      case AssertCmd(
                        PlainApp("=",
                                 `mainVar`,
                                 PlainApp("str.++", rawGroups @ _*))) => {
                        val groups =
                          for (PlainApp(s@FillVarName(FillNameIndex, _)) <- rawGroups)
                          yield s
                        Some(groups)
                      }
                      case _ =>
                        None
                    }
                  }

                  success(MatchOcc(start, regexInd, concatInd, matchInd,
                                   massageRegex(regex),
                                   mainVar, List(), groups, FillNameIndex))
                }

              case 1 =>
                chooseInt(start until matchInd) { concatInd =>
                  val (realMainVar, groups) = assumeIsDefined {
                    cmds(concatInd) match {
                      case AssertCmd(
                        PlainApp("=",
                                 realMainVar,
                                 `mainVar`)) => {
                        val PlainApp("str.++", rawGroups @ _*) = mainVar
                        val groups =
                          for (PlainApp(s@FillVarName(FillNameIndex, _)) <- rawGroups)
                          yield s
                        Some((realMainVar, groups))
                      }
                      case _ =>
                        None
                    }
                  }

                  success(MatchOcc(start, regexInd, concatInd, matchInd,
                                   massageRegex(regex),
                                   realMainVar, List(mainVar), groups, FillNameIndex))
                }
            }
          }

          case 1 => {
            val (regexInd, mainVar, regexTerm) = 
              findMaxInt[(Int, Term, Term)](start until matchInd) { regexInd =>
                val (mainVar, regexTerm) = assumeIsDefined(
                  cmds(regexInd) match {
                    case AssertCmd(
                      PlainApp("=",
                               PlainApp("str.in.re",
                                        mainVar, regexTerm),
                               PlainApp(MatchFlagName(`regex`, "0")))) =>
                      Some((mainVar, regexTerm))
                    case _ =>
                      None
                  })
                success((regexInd, mainVar, regexTerm))
              }

            // the |IsMatch_regex| flag has been used
            // only at the definition point
            assumeForall(start until regexInd) {
              ind => ind == regexInd || !matchOcc(cmds(ind))
            }

            // there is an equation in which the main variable is decomposed
            // into multiple string variables, corresponding to the capture
            // groups
            chooseInt(start until matchInd) { concatInd =>
              val groups = assumeIsDefined {
                cmds(concatInd) match {
                  case AssertCmd(
                      PlainApp("or",
                               PlainApp("not",
                               PlainApp("str.in.re",
                                        `mainVar`, `regexTerm`)),
                               CGEquation(`mainVar`, groups))) =>
                    Some(groups)
                  case _ =>
                    None
                }
              }

              success(MatchOcc(start, regexInd, concatInd, matchInd,
                               massageRegex(regex),
                               mainVar, List(), groups, FillNameIndex))
            }
          }
        }
      }
    }

  class CaptureGroupEquation(FillNameIndex : String) {
    def unapply(t : Term) : SOption[(Term, Seq[String])] = t match {
      case PlainApp("=",
                    mainVar,
                    PlainApp("str.++", rawGroups @ _*)) => {
        val groups =
          for (PlainApp(s@FillVarName(FillNameIndex, _)) <- rawGroups)
          yield s
        Some((mainVar, groups))
      }
      case PlainApp("=",
                    mainVar,
                    PlainApp(s@FillVarName(FillNameIndex, _))) =>
        Some((mainVar, List(s)))
      case _ =>
        None
    }
  }

  //////////////////////////////////////////////////////////////////////////////

  def massageRegex(regex : String) : String = {
    // unescape \\ and \|
    regex.replaceAll("\\x5C\\x5C", """\\""")
         .replaceAll("\\x5C\\x7C", "|")
  }

  //////////////////////////////////////////////////////////////////////////////

  // TODO: need to visit the children first?
  class SubstringReplacer(PrefixName  : String,
                          MainVar     : Term,
                          SuffixName  : String) extends ComposVisitor[Unit] {

    object Var4Def {
      def unapply(t : Term) : Boolean = t match {
        case  PlainApp("ite",
                      PlainApp(">=",
                               PlainApp("+",
                                        PlainApp("str.len", PlainApp(PrefixName)),
                                        rest3 @ _*),
                               IntLit(0)),
                      PlainApp("+",
                               PlainApp("str.len", PlainApp(PrefixName)),
                               rest4 @ _*),
                      PlainApp("ite",
                               PlainApp(">=",
                                        PlainApp("+",
                                                 PlainApp("str.len", PlainApp(PrefixName)),
//                                                 PlainApp("str.len", MainVar),
                                                 rest1 @ _*),
                                        IntLit(0)),
                               PlainApp("+",
                                        PlainApp("str.len", PlainApp(PrefixName)),
//                                        PlainApp("str.len", MainVar),
                                        rest2 @ _*),
                               IntLit(0)))
            if rest1 == rest2 && rest3 == rest4 => true
        case _ => false
      }
    }

    override def visit(p : FunctionTerm, arg : Unit) : Term = p match {
      case PlainApp("ite",
                    PlainApp("<=",
                             PlainApp("str.len", MainVar),
                             IntLit(0)),
                    StringLit(""),
                    PlainApp("str.substr",
                             MainVar,
                             IntLit(0),
                             PlainApp("ite",
                                      PlainApp(">=",
                                               PlainApp("+",
                                                        PlainApp("str.len", PlainApp(PrefixName)),
                                                        PlainApp("*",
                                                                 IntLit(-1),
                                                                 PlainApp("str.len", MainVar))),
                                               IntLit(0)),
                                      PlainApp("str.len", MainVar),
                                      PlainApp("str.len", PlainApp(PrefixName))))) =>
        PlainApp(PrefixName)

      // (let ((a!5 (ite (<= (+ (str.len X) (* (- 1) a!4)) 0)
      //                 ""
      //                 (str.substr X a!4 (+ (str.len X) (* (- 1) a!4))))))
      case PlainApp("ite",
                      PlainApp("<=",
                               PlainApp("+",
                                        PlainApp("str.len", MainVar),
                                        PlainApp("*", IntLit(-1), Var4Def())),
                               IntLit(0)),
                      StringLit(""),
                      PlainApp("str.substr",
                               MainVar,
                               Var4Def(),
                               PlainApp("+",
                                        PlainApp("str.len", MainVar),
                                        PlainApp("*", IntLit(-1), Var4Def())))) =>
        PlainApp(SuffixName)

      case p => super.visit(p, ())
    }

  }

  //////////////////////////////////////////////////////////////////////////////

  class ResultReplacer(PrefixName   : String,
                       SuffixName   : String,
                       cgVars       : Seq[String],
                       newVarPrefix : String) extends ComposVisitor[Unit] {

    val replacedTerms = new MHashMap[Term, String]

    override def visit(p : FunctionTerm, arg : Unit) : Term =
      super.visit(p, arg) match {
        case t@PlainApp("str.++", PlainApp(PrefixName), otherArgs @ _*)
            if (otherArgs.lastOption match {
                  case Some(PlainApp(SuffixName)) => true
                  case _                          => false
                }) &&
               (otherArgs forall {
                  case StringLit(_) => true
                  case PlainApp(x)  => cgVars contains x
                  case _            => false
                }) =>
          PlainApp(replacedTerms.getOrElseUpdate(
                     t, newVarPrefix + "_" + replacedTerms.size))
        case t => t
      }

  }

  def extractLengthSubst(replacedTerms : GMap[Term, String]) : Map[Term, Term] = {
    (for ((t, v) <- replacedTerms.iterator) yield {
       val PlainApp("str.++", args @ _*) = t
       (PlainApp("+", args map { x => PlainApp("str.len", x) } : _*),
        PlainApp("str.len", PlainApp(v)))
     }).toMap
  }

  //////////////////////////////////////////////////////////////////////////////

  class TermReplacer(subst : GMap[Term, Term]) extends ComposVisitor[Unit] {
    override def visit(p : FunctionTerm, arg : Unit) : Term =
      super.visit(p, arg) match {
        case newP if subst contains newP => subst(newP)
        case PlainApp("str.len", PlainApp("str.substr", _, _, len)) => len
        case newP => newP
      }
  }

}
