
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object RecFunElim {

  def apply(cmds : Seq[Command]) : Seq[Command] = (new RecFunElim)(cmds)

  val elimLeftTransducer = Parsing.parseString("""
    (define-funs-rec ((elimWhiteLeftRel  ((x String) (y String)) Bool)
                      (elimWhiteLeftHelp ((x String) (y String)) Bool)) (
      (or (and (= x "") (= y ""))
          (and (not (= x ""))
               (= (str.head_code x) (str.to_code " "))
               (elimWhiteLeftRel (str.tail x) y))
          (and (not (= x "")) (not (= y ""))
               (not (= (str.head_code x) (str.to_code " ")))
               (= (str.head x) (str.head y))
               (elimWhiteLeftHelp (str.tail x) (str.tail y))))
      (or (and (= x "") (= y ""))
          (and (not (= x "")) (not (= y ""))
               (= (str.head x) (str.head y))
               (elimWhiteLeftHelp (str.tail x) (str.tail y))))
    ))
    """)

  val elimLeftDefs = Parsing.parseString("""
    (define-fun elimWhiteLeft ((x String)) String
      (_eps ((y String)) (elimWhiteLeftRel x y)))
    """)

  val elimRightTransducer = Parsing.parseString("""
    (define-funs-rec ((elimWhiteRightRel   ((x String) (y String)) Bool)
                      (elimWhiteRightHelp1 ((x String) (y String)) Bool)
                      (elimWhiteRightHelp2 ((x String) (y String)) Bool)) (
      (or (and (= x "") (= y ""))
          (and (elimWhiteRightHelp1 x y))
          (and (elimWhiteRightHelp2 x y)))
      (or (and (not (= x "")) (not (= y ""))
               (= (str.head x) (str.head y))
               (elimWhiteRightHelp1 (str.tail x) (str.tail y)))
          (and (not (= x "")) (not (= y ""))
               (not (= (str.head_code x) (str.to_code " ")))
               (= (str.head x) (str.head y))
               (elimWhiteRightHelp2 (str.tail x) (str.tail y))))
      (or (and (= x "") (= y ""))
          (and (not (= x ""))
               (= (str.head_code x) (str.to_code " "))
               (elimWhiteRightHelp2 (str.tail x) y)))
    ))
    """)

  val elimRightDefs = Parsing.parseString("""
    (define-fun elimWhiteRight ((x String)) String
      (_eps ((y String)) (elimWhiteRightRel x y)))
    """)

  def addTransducer(benchmark      : Seq[Command],
                    actuallyAdd    : Boolean,
                    transducerDefs : Seq[Command],
                    funDefs        : Seq[Command]) : Seq[Command] =
    if (actuallyAdd) {
      Transducers.addDeclarations(
        Transducers.addTransducers(benchmark, transducerDefs), funDefs)
    } else {
      benchmark
    }

}

class RecFunElim extends ComposVisitor[Unit] {
  import RecFunElim._

  private var usedElimLeft, usedElimRight = false

  def apply(cmds : Seq[Command]) : Seq[Command] = {
    val res = cmds filterNot {
      case cmd : RecFunctionDefsCommand =>
        (printer print cmd) contains "str.repeat"
      case _ =>
        false
    }
    val res2 = for (cmd <- res) yield cmd.accept(this, ())
    val res3 =
      addTransducer(res2, usedElimLeft, elimLeftTransducer, elimLeftDefs)
    val res4 =
      addTransducer(res3, usedElimRight, elimRightTransducer, elimRightDefs)
    res4
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
