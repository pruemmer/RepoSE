
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object OpFixer extends ComposVisitor[Unit] {
  import ASTMatchers._

  def apply(cmds : Seq[Command]) : Seq[Command] =
    for (cmd <- cmds) yield cmd.accept(this, ())

  override def visit(p : IdentifierRef, arg : Unit) : SymbolRef = p match {
    case PlainSymbol("str.in_re") => PlainSymbol("str.in.re")
    case PlainSymbol("str.to_re") => PlainSymbol("str.to.re")
    case p                        => p
  }

}
