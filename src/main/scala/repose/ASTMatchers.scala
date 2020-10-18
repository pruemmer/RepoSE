
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object ASTMatchers {

  import scala.collection.JavaConversions.{asScalaBuffer, asScalaIterator}

  object FunApp {
    def unapplySeq(r : FunctionTerm) : SOption[Seq[AnyRef]] = {
      Some(List(r.symbolref_) ++ r.listterm_)
    }
  }

  object PlainSymbol {
    def unapply(s : SymbolRef) : scala.Option[String] = s match {
      case s : IdentifierRef => PlainIdentifier unapply s.identifier_
      case _ => None
    }
  }

  object PlainIdentifier {
    def unapply(id : Identifier) : scala.Option[String] = id match {
      case id : SymbolIdent => id.symbol_ match {
        case s : NormalSymbol =>
          Some(s.normalsymbolt_)
        case s : QuotedSymbol =>
          Some(s.quotedsymbolt_.substring(1, s.quotedsymbolt_.length - 1))
        case _ =>
          None
      }
      case _ => None
    }
  }
  
  object IndexedSymbol {
    def unapplySeq(s : SymbolRef) : scala.Option[Seq[String]] = s match {
      case s : IdentifierRef => IndexedIdentifier unapplySeq s.identifier_
      case _ => None
    }
  }

  object IndexedIdentifier {
    def unapplySeq(id : Identifier) : scala.Option[Seq[String]] = id match {
      case id : IndexIdent => id.symbol_ match {
        case s : NormalSymbol =>
          Some(List(s.normalsymbolt_) ++
               (id.listindexc_ map (_.asInstanceOf[Index].numeral_)))
        case _ => None
      }
      case _ => None
    }
  }

  object CastSymbol {
    def unapply(s : SymbolRef) : scala.Option[(String, Sort)] = s match {
      case s : CastIdentifierRef => s.identifier_ match {
        case id : SymbolIdent => id.symbol_ match {
          case ns : NormalSymbol => Some((ns.normalsymbolt_, s.sort_))
          case _ => None
        }
        case _ => None
      }
      case _ => None
    }
  }  

}
