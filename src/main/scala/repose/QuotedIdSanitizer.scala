
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object QuotedIdSanitizer extends ComposVisitor[Unit] {
  val printer = new PrettyPrinterNonStatic

  def apply(cmds : Seq[Command]) : Seq[Command] =
    for (cmd <- cmds) yield cmd.accept(this, ())

  override def visit(p : QuotedSymbol, arg : Unit) : Symbol = {
    val newName =
      p.quotedsymbolt_.replace("\\\\", "BS").replace("\\|", "ST")
    new QuotedSymbol (newName)
  }

}
