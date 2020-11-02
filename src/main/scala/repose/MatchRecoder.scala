
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import nd._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object MatchRecoder extends BacktrackingSearch {
  import RegexRecoder.ContainsSymbolVisitor
  import ASTMatchers._

  def apply(cmds : Seq[Command]) : Seq[Command] = {
    for (MatchOcc(_, _, regex) <- findOccurrence(cmds)) {
      println(Reg2PT(regex))
    }
    cmds
  }

  val FillVarName   = """([0-9]+) Fill ([0-9]+)""".r
  val MatchFlagName = """IsMatch_/(.+)/_([0-9]+)""".r

  val printer = new PrettyPrinterNonStatic

  case class MatchOcc(start : Int, end : Int, regex : String)

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

        println(regex)

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

          println(mainVar)
          println(groups)

          success(MatchOcc(start, matchInd, regex))
        }
      }
    }

  abstract class FindDataVisitor[Data]
           extends FoldVisitor[SOption[Data], Unit] {
    def apply(cmd : Command) : SOption[Data] = cmd.accept(this, ())
    def leaf(arg : Unit) = None
    def combine(x : SOption[Data], y : SOption[Data], arg : Unit) = x orElse y
  }

  object FindSymbolVisitor {
    def apply[Data](cmd : Command)
                   (pred : String => SOption[Data]) : SOption[Data] =
      (new FindSymbolVisitor(pred))(cmd)
  }

  class FindSymbolVisitor[Data](pred : String => SOption[Data])
        extends FindDataVisitor[Data] {
    override def visit(p : NormalSymbol, arg : Unit) : SOption[Data] =
      pred(p.normalsymbolt_)
    override def visit(p : QuotedSymbol, arg : Unit) : SOption[Data] =
      pred(p.quotedsymbolt_.substring(1, p.quotedsymbolt_.size - 1))
  }

}
