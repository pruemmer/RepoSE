
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object ASTMatchers {

  import scala.collection.JavaConversions.{asScalaBuffer, asScalaIterator}

  object FunApp {
    def unapplySeq(r : FunctionTerm) : SOption[(SymbolRef, Seq[Term])] = {
      Some((r.symbolref_, r.listterm_))
    }
  }

  object PlainApp {
    def apply(f : String, args : Term*) : FunctionTerm = {
      val termArgs = new ListTerm()
      for (a <- args)
        termArgs.add(a)
      new FunctionTerm(PlainSymbol(f), termArgs)
    }
    def unapplySeq(r : FunctionTerm) : SOption[(String, Seq[Term])] = r match {
      case FunApp(PlainSymbol(str), rest @ _*) => Some(str, rest)
      case _ => None
    }
  }

  object IntLit {
    def unapply(t : AnyRef) : SOption[Int] = t match {
      case t : ConstantTerm  => t.specconstant_ match {
        case c : NumConstant => Some(c.numeral_.toInt)
        case _ => None
      }
      case PlainApp("-", IntLit(v)) => Some(-v)
      case _ => None
    }
  }

  object PlainSymbol {
    def apply(s : String) : SymbolRef =
      new IdentifierRef(new SymbolIdent(new NormalSymbol(s)))
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
