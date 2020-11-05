
package repose;

object Main {

  def main(args: Array[String]) : Unit = {
    val trans = new Transformer
    for (inputFile <- args)
      try {
        val outputFile = inputFile + "-processed.smt2"
        trans.transform(inputFile, outputFile)
      } catch {
        case e : Exception =>
          Console.err.println(e.getMessage)
      }
  }

}
