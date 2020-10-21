
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object RegexRecoder extends ComposVisitor[Unit] {
  val printer = new PrettyPrinterNonStatic

  import ASTMatchers._

  def apply(cmds : Seq[Command]) : Seq[Command] =
    for (cmd <- cmds;
         if !((printer print cmd) contains " Fill "))
    yield cmd

}
