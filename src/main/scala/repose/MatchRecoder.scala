
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import nd._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object MatchRecoder extends BacktrackingSearch {
  import RegexRecoder.ContainsSymbolVisitor

  val FillVarName = """\|([0-9]+) Fill ([0-9]+)\|""".r

  val printer = new PrettyPrinterNonStatic

  case class MatchOcc(start : Int, end : Int)

  def findOccurrence(cmds : Seq[Command]) : SOption[MatchOcc] =
    search[MatchOcc] {
      chooseMinInt(0 until cmds.size) { start =>
        val cmd = cmds(start)
        assume(cmd.isInstanceOf[AssertCommand])
        val fillNameIndex = assumeIsDefined {
          FindSymbolVisitor(cmd) {
            case FillVarName(num1, "0") => Some(num1.toInt)
            case _                      => None
          }
        }
        println(fillNameIndex)
        success(MatchOcc(start, start + 1))
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
      pred(p.quotedsymbolt_)
  }

}
