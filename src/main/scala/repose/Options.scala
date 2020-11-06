
package repose;

object Options {

  object MatchEncoding extends Enumeration {
    val PrioTransducer, RegexTerm = Value
  }

  var matchEncoding : MatchEncoding.Value = MatchEncoding.PrioTransducer

  var filenameSuffix : String = "processed"

}
