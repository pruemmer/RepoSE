
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import sys.process._

object Reg2PT {
  val printer = new PrettyPrinterNonStatic

  def reg2PTCommand(regex : String, prefix : String) = {
    val executable =
      sys.env.get("BASEDIR") match {
        case Some(path) => path + "/bin/reg2pt"
        case None => "reg2pt"
      }

    executable + " -s " + prefix + " \'" + regex + "\'"
//    "java -jar lib/reg2pt.jar -s \'" + regex + "\'"
  }

  def apply(regex : String, prefix : String) : (Seq[String], Seq[Command]) = {
    val output = new StringBuffer

    (reg2PTCommand(regex, prefix) run BasicIO(false, output, None)).exitValue match {
      case 0 => {
        val allCmds = Parsing.parseString(output.toString)
        val cmds    = allCmds filter (_.isInstanceOf[RecFunctionDefsCommand])
        (for (n <- 1 to cmds.size) yield (prefix + n + "main"), cmds)
      }
      case err =>
        throw new Exception("reg2pt failed, exit value " + err)
    }
  }

}
