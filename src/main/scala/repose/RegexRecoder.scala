
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object RegexRecoder {
  val printer = new PrettyPrinterNonStatic

  val fillVarName = " Fill "

  import ASTMatchers._

  def apply(cmds : Seq[Command]) : Seq[Command] = {
    var nextId = -1
    var nextFillString = ""
    var nextVisitor : ContainsSymbolVisitor = null

    def incId = {
      nextId = nextId + 1
      nextFillString = "|" + nextId + " Fill 0|"
      nextVisitor = new ContainsSymbolVisitor(_ == nextFillString)
    }

    incId

    var withinRegexCode = false

    for (cmd <- cmds;
         if (cmd match {
               case cmd : FunctionDeclCommand if ContainsFillVisitor(cmd) =>
                 false
               case cmd if nextVisitor(cmd) => {
                 withinRegexCode = true
                 false
               }
               case cmd if withinRegexCode =>
                 if (ContainsFillVisitor(cmd)) {
                   false
                 } else {
                   withinRegexCode = false
                   incId
                   true
                 }
               case _ =>
                 true
         }))
    yield cmd
  }

  object ContainsSymbolVisitor {
    def apply(cmd : Command)(pred : String => Boolean) : Boolean =
      (new ContainsSymbolVisitor(pred))(cmd)
  }

  class ContainsSymbolVisitor(pred : String => Boolean)
        extends FoldVisitor[Boolean, Unit] {
    def apply(cmd : Command) : Boolean =
      cmd.accept(this, ())

    def leaf(arg : Unit) = false
    def combine(x : Boolean, y : Boolean, arg : Unit) = x || y

    override def visit(p : NormalSymbol, arg : Unit) : Boolean =
      pred(p.normalsymbolt_)
    override def visit(p : QuotedSymbol, arg : Unit) : Boolean =
      pred(p.quotedsymbolt_)
  }

  val ContainsFillVisitor =
    new ContainsSymbolVisitor(str => str contains fillVarName)

}
