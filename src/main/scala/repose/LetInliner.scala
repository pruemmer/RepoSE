
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object LetInliner {
  import ASTMatchers._

  // TODO: apply until a fixed-point is reached?
  def apply(cmds : Seq[Command]) : Seq[Command] =
    for (cmd <- cmds) yield cmd.accept(RecursiveInliner, ())

  object OccCounter extends FoldVisitor[Map[String, Int], Unit] {
    def leaf(arg : Unit) = Map()
    def combine(x : Map[String, Int], y : Map[String, Int], arg : Unit) =
      (for (str <- (x.keySet ++ y.keySet).iterator)
       yield (str -> (x.getOrElse(str, 0) + y.getOrElse(str, 0)))).toMap

    override def visit(p : NullaryTerm,
                       arg : Unit) : Map[String, Int] = p match {
      case PlainApp(str) => Map(str -> 1)
      case _             => Map()
    }
  }

  class Inliner(subst : Map[String, Term])
        extends ComposVisitor[Unit] {
    override def visit(p : NullaryTerm, arg : Unit) : Term = p match {
      case PlainApp(str) => subst.getOrElse(str, p)
      case p             => p
    }
  }

  object RecursiveInliner extends ComposVisitor[Unit] {
    override def visit(p : LetTerm, arg : Unit) : Term =
      super.visit(p, arg) match {
        case p@Let(inner, defs @ _*) => {
          val occs = inner.accept(OccCounter, ())
          val subst =
            (for ((v, t) <- defs; if occs.getOrElse(v, 0) <= 3)
             yield (v -> t)).toMap

          if (subst.isEmpty) {
            p
          } else {
            val newInner = inner.accept(new Inliner (subst), ())
            (for ((v, t) <- defs; if !(subst contains v)) yield (v, t)) match {
              case Seq() => newInner
              case newDefs => Let(newInner, newDefs : _*)
            }
          }
        }
        case p => p
      }
  }

}
