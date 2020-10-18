
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

class Transformer {

  val printer = new PrettyPrinterNonStatic

  def transform(inputFile : String, outputFile : String) : Unit = {
    Console.err.println("Rewriting \"" + inputFile +
                          "\" to \"" + outputFile + "\" ...")

    val input =
      new java.io.BufferedReader (
        new java.io.FileReader(new java.io.File (inputFile)))
    val l = new Yylex(input)
    val p = new parser(l) {
      override def report_error(message : String, info : Object) : Unit = {
        Console.err.println(message)
      }
    }

    val script = p.pScriptC.asInstanceOf[Script]
    val script2 = RecFunElim.visit(script, ())

    val out = new java.io.FileOutputStream(outputFile)
    Console.withOut(out) {
      println(printer print script2)
    }

    out.close
  }

}
