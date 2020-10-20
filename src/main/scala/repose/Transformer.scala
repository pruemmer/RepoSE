
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

class Transformer {

  import scala.collection.JavaConversions.{asScalaBuffer, asScalaIterator}

  val printer = new PrettyPrinterNonStatic

  def transform(inputFile : String, outputFile : String) : Unit = {
    Console.err.println("Rewriting \"" + inputFile +
                          "\" to \"" + outputFile + "\" ...")

    val script = Parsing.parseFromFile(inputFile)
    val script2 = RecFunElim(script)
    val script3 = QuotedIdSanitizer(script2)

    val out = new java.io.FileOutputStream(outputFile)
    Console.withOut(out) {
      printLineByLine(script3)
    }
    out.close
  }

  def printLineByLine(script : Seq[Command]) : Unit = {
    for (cmd <- script)
      println(printer print cmd)
  }

}
