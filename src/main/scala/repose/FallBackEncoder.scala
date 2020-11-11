
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import nd._

import scala.{Option => SOption}
import scala.collection.JavaConverters._
import scala.collection.mutable.{HashSet => MHashSet, Set => MSet}

object FallBackRecoder extends BacktrackingSearch {
  import ASTMatchers._
  import Constants._

  def apply(cmds : Seq[Command]) : Seq[Command] = {
    var num            = 0
    var curCmds        = cmds
    val coveredIndexes = new MHashSet[String]

    var cont           = true
    while (cont)
      findOccurrence(curCmds, coveredIndexes) match {
        case Some(occ) => {
          curCmds = recode(curCmds, occ, num)
          coveredIndexes += occ.fillNameIndex
          num = num + 1
        }
        case None =>
          cont = false
      }

    curCmds
  }

  val printer = new PrettyPrinterNonStatic

  case class MatchOcc(startInd : Int, endInd : Int, fillNameIndex : String)

  def recode(cmds : Seq[Command], occ : MatchOcc, num : Int) : Seq[Command] = {
    println("Rewriting fall-back: " + occ)

    val MatchOcc(startInd, endInd, fillNameIndex) = occ
    var newCmds = cmds
    newCmds =
      newCmds.patch(startInd, List(), endInd - startInd + 1)
    newCmds =
      newCmds filterNot {
        cmd => !cmd.isInstanceOf[AssertCommand] &&
               containsFillNameIndex(cmd, fillNameIndex)
      }
    newCmds
  }

  def containsFillNameIndex(cmd : Command, FillNameIndex : String) =
    ContainsSymbolVisitor(cmd) {
      case FillVarName(FillNameIndex, _) => true
      case _ => false
    }

  def findOccurrence(cmds : Seq[Command],
                     coveredIndexes : MSet[String]) : SOption[MatchOcc] =
    search[MatchOcc] {
      chooseInt(0 until cmds.size) { start =>
        assume(cmds(start).isInstanceOf[AssertCommand])

        val FillNameIndex = assumeIsDefined {
          FindSymbolVisitor(cmds(start)) {
            case FillVarName(num1, "0") => Some(num1)
            case _                      => None
          }
        }

        assume(!(coveredIndexes contains FillNameIndex))

        assumeForall(0 until cmds.size) {
          ind => cmds(ind) != markerAssertion(FillNameIndex)
        }

        def containsFill(ind : Int) = {
          cmds(ind).isInstanceOf[AssertCommand] &&
          containsFillNameIndex(cmds(ind), FillNameIndex)
        }

        def assumeNoFill(b : Int, e : Int) =
          assumeForall(b until e) { ind => !containsFill(ind) }

        assumeNoFill(0, start)

        chooseInt((start + 1) until cmds.size) { end =>
          assumeForall(start to end) { ind => containsFill(ind) }

          assume(end + 1 >= cmds.size || !containsFill(end + 1))

          success(MatchOcc(start, end, FillNameIndex))
        }
      }
    }

}
