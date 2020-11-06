
package repose;

object Main {

  def main(args: Array[String]) : Unit = {
    val trans = new Transformer
    for (argument <- args)
      try {
        argument match {
          case "-matchEncoding=prioTransducer" =>
            Options.matchEncoding = Options.MatchEncoding.PrioTransducer
          case "-matchEncoding=regexTerm" =>
            Options.matchEncoding = Options.MatchEncoding.RegexTerm
          case inputFile => {
            val outputFile = inputFile + "-processed.smt2"
            trans.transform(inputFile, outputFile)
          }
        }
      } catch {
        case e : Exception =>
          Console.err.println(e.getMessage)
      }
  }

}
