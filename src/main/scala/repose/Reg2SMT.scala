
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import sys.process._

object Reg2SMT {
  def reg2SMTCommand(regex : String) = {
    val executable =
      sys.env.get("BASEDIR") match {
        case Some(path) => path + "/bin/reg2smt"
        case None => "reg2smt"
      }

    Seq(executable, "-s", regex)
  }

  def apply(regex : String) : Term = {
    val output = new StringBuffer

    (reg2SMTCommand(regex) run BasicIO(false, output, None)).exitValue match {
      case 0 =>
        Parsing.parseExpression(output.toString)
      case err =>
        throw new Exception("reg2smt failed, exit value " + err)
    }
  }

}
