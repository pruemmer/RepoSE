

package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

object Parsing {

  import scala.collection.JavaConversions.{asScalaBuffer, asScalaIterator}

  def parseFromFile(inputFile : String) : Seq[Command] = {
    val input =
      new java.io.BufferedReader (
        new java.io.FileReader(new java.io.File (inputFile)))
    val res = parseCommands(input)
    input.close
    res
  }

  def parseString(inputString : String) : Seq[Command] = {
    val input =
      new java.io.BufferedReader (new java.io.StringReader(inputString))
    val res = parseCommands(input)
    input.close
    res
  }

  def parseCommands(input : java.io.Reader) : Seq[Command] = {
    val l = new Yylex(input)
    val p = new parser(l) {
      override def report_error(message : String, info : Object) : Unit = {
        Console.err.println(message)
      }
    }

    p.pScriptC.asInstanceOf[Script].listcommand_
  }

  def parseExpression(input : String) : Term = {
    parseString("(ignore " + input + ")") match {
      case Seq(cmd : IgnoreCommand) =>
        cmd.term_
      case _ =>
        throw new Exception("Parsing of expression failed")
    }
  }

}
