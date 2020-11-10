
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import nd._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

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

  case class MatchOcc(startInd      : Int,
                      membershipInd : Int,
                      concatInd     : Int,
                      matchInd      : Int,
                      regex         : String,
                      stringVar     : String,
                      cgVars        : Seq[String],
                      fillNameIndex : String)

  def recode(cmds : Seq[Command], occ : MatchOcc, num : Int) : Seq[Command] = {
    import occ._

    Options.matchEncoding match {
      case Options.MatchEncoding.PrioTransducer => {
        val (preTransducerFuns, preTransducerDefs) = Reg2PT(regex, "MatchTD_" + num + "_")

        val (transducerFuns, transducerDefs) =
          (cgVars.size - preTransducerFuns.size) match {
            case 0 => (preTransducerFuns, preTransducerDefs)
            case 2 => Reg2PT("(.*?)" + regex + "(.*)", "MatchTD_" + num + "_")
          }

        var newCmds = cmds
        val transducerApps =
          for ((cgVar, tdFun) <- cgVars zip transducerFuns)
          yield AssertCmd(PlainApp(tdFun, PlainApp(stringVar), PlainApp(cgVar)))

        newCmds = newCmds.patch(concatInd, transducerApps, 1)
        newCmds = newCmds.patch(startInd, List(markerAssertion(fillNameIndex)),
                                membershipInd - startInd)

        if (cgVars.size == preTransducerFuns.size + 2) {
          val visitor = new SubstringReplacer(cgVars.head, stringVar, cgVars.last)
          newCmds = newCmds map (_.accept(visitor, ()))
        }

        Transducers.addTransducers(newCmds, transducerDefs)
      }

      case Options.MatchEncoding.RegexTerm => {
        val regexTerm = Reg2SMT(regex)
        var newCmds = cmds

        val regexVar = "re!1"
        val extractors =
          for ((cgVar, n) <- cgVars.zipWithIndex)
          yield PlainApp("=",
                         PlainApp(cgVar),
                         FunApp(IndexedSymbol("str.extract", (n+1).toString),
                                PlainApp(stringVar), PlainApp(regexVar)))
        val extractorConj =
          AssertCmd(Let(PlainApp("and", extractors : _*),(regexVar, regexTerm)))

        newCmds = newCmds.patch(concatInd, List(extractorConj), 1)
        newCmds = newCmds.patch(startInd, List(markerAssertion(fillNameIndex)),
                                membershipInd - startInd)

        newCmds
      }
    }
  }

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
        val (matchInd, regex) =
          findMinInt[(Int, String)]((start + 1) until cmds.size) { end =>
            val regex = assumeIsDefined {
              cmds(end) match {
                case AssertCmd(PlainApp(MatchFlagName(regex, "0"))) =>
                  Some(regex)
                case _ =>
                  None
              }
            }
            success((end, regex))
          }

        def matchOcc(cmd : Command) =
          ContainsSymbolVisitor(cmd) {
            case MatchFlagName(`regex`, "0") => true
            case _ => false
          }

        alternatives(2) {
          case 0 => {
            // there is only one occurrence of the |IsMatch_regex| flag
            assumeForall(start until matchInd) {
              ind => !matchOcc(cmds(ind))
            }

            // there is an equation in which the main variable is decomposed
            // into multiple string variables, corresponding to the capture
            // groups
            chooseInt(start until matchInd) { concatInd =>
              val (mainVar, groups) = assumeIsDefined {
                cmds(concatInd) match {
                  case AssertCmd(PlainApp("=",
                                          PlainApp(mainVar),
                                          PlainApp("str.++", rawGroups @ _*))) => {
                    val groups =
                      for (PlainApp(s@FillVarName(FillNameIndex, _)) <- rawGroups)
                      yield s
                    Some((mainVar, groups))
                  }
                  case _ =>
                    None
                }
              }

              success(MatchOcc(start, concatInd - 1, concatInd, matchInd,
                               massageRegex(regex),
                               mainVar, groups, FillNameIndex))
            }
          }

          case 1 => chooseInt(start until matchInd) { matchDefInd =>
            val (mainVar, regexTerm) = assumeIsDefined(
              cmds(matchDefInd) match {
                case AssertCmd(
                  PlainApp("=",
                           PlainApp("str.in.re", PlainApp(mainVar), regexTerm),
                           PlainApp(MatchFlagName(`regex`, "0")))) =>
                  Some((mainVar, regexTerm))
                case _ =>
                  None
              })

            // the |IsMatch_regex| flag has been used
            // only at the definition point
            assumeForall(start until matchDefInd) {
              ind => ind == matchDefInd || !matchOcc(cmds(ind))
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
                                        PlainApp(`mainVar`), `regexTerm`)),
                               CGEquation(`mainVar`, groups))) =>
                    Some(groups)
                  case _ =>
                    None
                }
              }

              success(MatchOcc(start, concatInd - 1, concatInd, matchInd,
                               massageRegex(regex),
                               mainVar, groups, FillNameIndex))
            }
          }
        }
      }
    }

  class CaptureGroupEquation(FillNameIndex : String) {
    def unapply(t : Term) : SOption[(String, Seq[String])] = t match {
      case PlainApp("=",
                    PlainApp(mainVar),
                    PlainApp("str.++", rawGroups @ _*)) => {
        val groups =
          for (PlainApp(s@FillVarName(FillNameIndex, _)) <- rawGroups)
          yield s
        Some((mainVar, groups))
      }
      case PlainApp("=",
                    PlainApp(mainVar),
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

  // TODO: need to visit the children first?
  class SubstringReplacer(PrefixName  : String,
                          MainVarName : String,
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
                                                 PlainApp("str.len", PlainApp(MainVarName)),
                                                 rest1 @ _*),
                                        IntLit(0)),
                               PlainApp("+",
                                        PlainApp("str.len", PlainApp(PrefixName)),
                                        PlainApp("str.len", PlainApp(MainVarName)),
                                        rest2 @ _*),
                               IntLit(0)))
            if rest1 == rest2 && rest3 == rest4 => true
        case _ => false
      }
    }

    override def visit(p : FunctionTerm, arg : Unit) : Term = p match {
      case PlainApp("ite",
                    PlainApp("<=",
                             PlainApp("str.len", PlainApp(MainVarName)),
                             IntLit(0)),
                    StringLit(""),
                    PlainApp("str.substr",
                             PlainApp(MainVarName),
                             IntLit(0),
                             PlainApp("ite",
                                      PlainApp(">=",
                                               PlainApp("+",
                                                        PlainApp("str.len", PlainApp(PrefixName)),
                                                        PlainApp("*",
                                                                 IntLit(-1),
                                                                 PlainApp("str.len", PlainApp(MainVarName)))),
                                               IntLit(0)),
                                      PlainApp("str.len", PlainApp(MainVarName)),
                                      PlainApp("str.len", PlainApp(PrefixName))))) =>
        PlainApp(PrefixName)

      // (let ((a!5 (ite (<= (+ (str.len X) (* (- 1) a!4)) 0)
      //                 ""
      //                 (str.substr X a!4 (+ (str.len X) (* (- 1) a!4))))))
      case PlainApp("ite",
                      PlainApp("<=",
                               PlainApp("+",
                                        PlainApp("str.len", PlainApp(MainVarName)),
                                        PlainApp("*", IntLit(-1), Var4Def())),
                               IntLit(0)),
                      StringLit(""),
                      PlainApp("str.substr",
                               PlainApp(MainVarName),
                               Var4Def(),
                               PlainApp("+",
                                        PlainApp("str.len", PlainApp(MainVarName)),
                                        PlainApp("*", IntLit(-1), Var4Def())))) =>
        PlainApp(SuffixName)

      case p => super.visit(p, ())
    }

  }

}
