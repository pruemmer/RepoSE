
package repose;

object Main extends App {
  val trans = new Transformer

  trans.transform("test/glob_78.smt2",
                  "test/glob_78_rewritten.smt2")
}
