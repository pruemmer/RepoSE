
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object RecFunElim {

  def apply(cmds : Seq[Command]) : Seq[Command] = (new RecFunElim)(cmds)

}

class RecFunElim extends ComposVisitor[Unit] {

  private var usedElimLeft, usedElimRight = false

  def apply(cmds : Seq[Command]) : Seq[Command] = {
    for (cmd <- cmds) yield cmd.accept(this, ())
  }

  val printer = new PrettyPrinterNonStatic

  import ASTMatchers._

  object WhiteLeft {
    def unapply(t : AnyRef) : SOption[AnyRef] = t match {
      case FunApp(IndexedSymbol("str.whiteLeft", "0"), t, IntLit(0)) => Some(t)
      case _ => None
    }
  }

  object WhiteRight {
    def unapply(t : AnyRef) : SOption[AnyRef] = t match {
      case FunApp(IndexedSymbol("str.whiteRight", "0"),
                  t1, PlainApp("str.len", t2))
          if t1 == t2 => Some(t1)
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
          if t1 == t2 && t1 == t3 && t1 == t4 => {
        usedElimLeft = true
        PlainApp("elimWhiteLeft", t1)
      }
      case PlainApp("str.substr",
                    t1,
                    IntLit(0),
                    PlainApp("+", IntLit(1), WhiteRight(t2)))
          if t1 == t2 => {
        usedElimRight = true
        PlainApp("elimWhiteRight", t1)
      }
      case _ =>
        super.visit(p, arg)
    }
  }

}
