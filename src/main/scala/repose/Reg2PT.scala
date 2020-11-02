
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import sys.process._

object Reg2PT {
  val printer = new PrettyPrinterNonStatic

  def reg2PTCommand(regex : String) =
    "java -jar lib/reg2pt.jar -s \'" + regex + "\'"

  def apply(regex : String) : (Seq[String], Seq[Command]) = {
    val output = new StringBuffer

    (reg2PTCommand(regex) run BasicIO(false, output, None)).exitValue match {
      case 0 => {
        val allCmds = Parsing.parseString(output.toString)
        val cmds    = allCmds filter (_.isInstanceOf[RecFunctionDefsCommand])
        (List("f_s0i"), cmds)
      }
      case err =>
        throw new Exception("reg2pt failed, exit value " + err)
    }
  }

}
