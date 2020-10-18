
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object RecFunElim extends ComposVisitor[Unit] {

  val printer = new PrettyPrinterNonStatic

  import ASTMatchers._

  override def visit(p : FunctionTerm, arg : Unit) : Term = {
    p match {
      case FunApp(PlainSymbol("str.substr"), _*) =>
        println(printer print p)
      case _ =>
        // nothing
    }
    super.visit(p, arg)
  }

}
