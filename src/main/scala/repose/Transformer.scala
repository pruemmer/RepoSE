
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

    val script  = Parsing.parseFromFile(inputFile)
    val script2 = OpFixer(script)
    val script3 = RecFunElim(script2)
    val script4 = MatchRecoder(script3)
    val script5 = FallBackRecoder(script4)
    val script6 = QuotedIdSanitizer(script5)

    val out = new java.io.FileOutputStream(outputFile)
    Console.withOut(out) {
      printLineByLine(script6)
    }
    out.close
  }

  def printLineByLine(script : Seq[Command]) : Unit = {
    for (cmd <- script;
         if !(ASTMatchers.ContainsSymbolVisitor(cmd) {
                case Constants.EncodingMarkerName() => true
                case _ => false
              }))
      println(printer print cmd)
  }

}
