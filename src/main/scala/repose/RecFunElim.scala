
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object RecFunElim extends ComposVisitor[Unit] {

  val printer = new PrettyPrinterNonStatic

  import ASTMatchers._

  object WhiteLeft {
    def unapply(t : AnyRef) : SOption[AnyRef] = t match {
      case FunApp(IndexedSymbol("str.whiteLeft", "0"), t, IntLit(0)) => Some(t)
      case _ => None
    }
  }

  override def visit(p : FunctionTerm, arg : Unit) : Term = {
    p match {
      case PlainApp("str.substr",
                    t1,
                    WhiteLeft(t2),
                    PlainApp("+",
                             PlainApp("*", IntLit(-1), WhiteLeft(t3)),
                             PlainApp("str.len", t4)))
          if t1 == t2 && t1 == t3 && t1 == t4 =>
        PlainApp("elimWhiteLeft", t1)
      case _ =>
        super.visit(p, arg)
    }
  }

}
