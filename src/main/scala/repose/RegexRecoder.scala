
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object RegexRecoder extends ComposVisitor[Unit] {
  val printer = new PrettyPrinterNonStatic

  val fillVarName = " Fill "

  import ASTMatchers._

  def apply(cmds : Seq[Command]) : Seq[Command] = {
    var withinRegexCode = false

    for (cmd <- cmds;
         if (cmd match {
               case FunctionDecl(name, _, _)
                   if ((printer print name) contains fillVarName) =>
                 false
               case Assert(PlainApp("str.in.re", name, _))
                   if ((printer print name) contains fillVarName) => {
                 withinRegexCode = true
                 false
               }
               case cmd if withinRegexCode =>
                 if ((printer print cmd) contains fillVarName) {
                   false
                 } else {
                   withinRegexCode = false
                   true
                 }
               case _ =>
                 true
         }))
    yield cmd
  }

}
