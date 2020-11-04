
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import nd._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object Constants {

  val FillVarName   = """([0-9]+) Fill ([0-9]+)""".r
  val MatchFlagName = """IsMatch_/(.+)/_([0-9]+)""".r

  val fillVarName = " Fill "

  import ASTMatchers._
  val MarkerAssertion = AssertCmd(PlainApp("EncodingMarker"))

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

  case class MatchOcc(startInd : Int,
                      membershipInd : Int,
                      concatInd : Int,
                      matchInd : Int,
                      regex : String,
                      stringVar : String,
                      cgVars : Seq[String])

  def recode(cmds : Seq[Command], occ : MatchOcc, num : Int) : Seq[Command] = {
    val MatchOcc(startInd, membershipInd, concatInd, _,
                 regex, stringVar, cpVars) = occ
    val (transducerFuns, transducerDefs) = Reg2PT(regex, "MatchTD_" + num + "_")
    assert(transducerFuns.size == cpVars.size)
    var newCmds = cmds
    val transducerApps =
      for ((cpVar, tdFun) <- cpVars zip transducerFuns)
      yield AssertCmd(PlainApp(tdFun, PlainApp(stringVar), PlainApp(cpVar)))

    newCmds = newCmds.patch(concatInd, transducerApps, 1)
    newCmds = newCmds.patch(startInd, List(), membershipInd - startInd)

    Transducers.addTransducers(newCmds, transducerDefs)
  }

  def findOccurrence(cmds : Seq[Command]) : SOption[MatchOcc] =
    search[MatchOcc] {
      chooseInt(0 until cmds.size) { start =>
        val startCmd = cmds(start)
        assume(startCmd.isInstanceOf[AssertCommand])

        val FillNameIndex = assumeIsDefined {
          FindSymbolVisitor(startCmd) {
            case FillVarName(num1, "0") => Some(num1)
            case _                      => None
          }
        }

        assumeForall(0 until start) { ind =>
          !(cmds(ind).isInstanceOf[AssertCommand] &&
              ContainsSymbolVisitor(cmds(ind)) {
                case FillVarName(FillNameIndex, _) => true
                case _ => false
              })
        }

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
                           mainVar, groups))
        }
      }
    }

}
