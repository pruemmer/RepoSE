

package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

object Transducers {

  val Seq(transducerOption) =
    Parsing.parseString("(set-option :parse-transducers true)")

  def addTransducers(benchmark : Seq[Command],
                     transducers : Seq[Command]) : Seq[Command] =
    (benchmark indexOf transducerOption) match {
      case -1 =>
        List(transducerOption) ++ transducers ++ benchmark
      case ind =>
        benchmark.patch(ind + 1, transducers, 0)
    }

  def addDeclarations(benchmark : Seq[Command],
                      decls : Seq[Command]) : Seq[Command] =
    (benchmark indexWhere {
       case _ : SetOptionCommand => false
       case _ : RecFunctionDefCommand => false
       case _ : RecFunctionDefsCommand => false
       case _ => true
     }) match {
      case -1 =>
        benchmark ++ decls
      case ind =>
        benchmark.patch(ind, decls, 0)
    }
}
