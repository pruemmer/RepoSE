
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
        val (transducerFuns, transducerDefs) =
          Reg2PT(regex, "MatchTD_" + num + "_")
        assert(transducerFuns.size == cgVars.size)
        var newCmds = cmds
        val transducerApps =
          for ((cgVar, tdFun) <- cgVars zip transducerFuns)
          yield AssertCmd(PlainApp(tdFun, PlainApp(stringVar), PlainApp(cgVar)))

        newCmds = newCmds.patch(concatInd, transducerApps, 1)
        newCmds = newCmds.patch(startInd, List(markerAssertion(fillNameIndex)),
                                membershipInd - startInd)

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

        // there is only one occurrence of the |IsMatch_regex| flag 
        assumeForall(start until matchInd) { ind =>
          !(ContainsSymbolVisitor(cmds(ind)) {
              case MatchFlagName(`regex`, "0") => true
              case _ => false
            })
        }

        // there is an equation in which the main variable is decomposed
        // into multiple string variables, corresponding to the capture groups
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

          success(MatchOcc(start, concatInd - 1, concatInd, matchInd, regex,
                           mainVar, groups, FillNameIndex))
        }
      }
    }

  def massageRegex(regex : String) : String =
    regex

}
