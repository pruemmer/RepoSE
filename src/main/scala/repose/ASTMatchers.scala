
package repose;

import ap.parser.smtlib._
import ap.parser.smtlib.Absyn._

import scala.{Option => SOption}
import scala.collection.JavaConverters._

object ASTMatchers {

  private val SaneId =
    """[+-/*=%?!.$_~&^<>@a-zA-Z][+-/*=%?!.$_~&^<>@a-zA-Z0-9]*""".r
  
  def quoteIdentifier(str : String) = str match {
    case SaneId() => str
    case _        => "|" + str.replace("|", "") + "|"
  }

  import scala.collection.JavaConversions.{asScalaBuffer, asScalaIterator}

  object FunApp {
    def apply(f : SymbolRef, args : Term*) : Term =
      if (args.isEmpty) {
        new NullaryTerm(f)
      } else {
        val termArgs = new ListTerm()
        for (a <- args)
          termArgs.add(a)
        new FunctionTerm(f, termArgs)
      }
    def unapplySeq(r : Term) : SOption[(SymbolRef, Seq[Term])] = r match {
      case r : FunctionTerm =>
        Some((r.symbolref_, r.listterm_))
      case r : NullaryTerm =>
        Some((r.symbolref_, List()))
      case _ =>
        None
    }
  }

  object PlainApp {
    def apply(f : String, args : Term*) : Term =
      FunApp(PlainSymbol(f), args : _*)
    def unapplySeq(r : Term) : SOption[(String, Seq[Term])] = r match {
      case FunApp(PlainSymbol(str), rest @ _*) => Some(str, rest)
      case _ => None
    }
  }

  object IntLit {
    def apply(n : Int) : Term =
      if (n < 0)
        PlainApp("-", new ConstantTerm (new NumConstant ((-n).toString)))
      else
        new ConstantTerm (new NumConstant (n.toString))

    def unapply(t : AnyRef) : SOption[Int] = t match {
      case t : ConstantTerm  => t.specconstant_ match {
        case c : NumConstant => Some(c.numeral_.toInt)
        case _ => None
      }
      case PlainApp("-", IntLit(v)) => Some(-v)
      case _ => None
    }
  }

  object StringLit {
    def apply(s : String) : Term =
      new ConstantTerm (new StringConstant ("\"" + s + "\""))
    def unapply(t : ConstantTerm) : SOption[String] = t match {
      case t : ConstantTerm  => t.specconstant_ match {
        case c : StringConstant =>
          Some(c.smtstring_.substring(1, c.smtstring_.length - 1))
        case _ => None
      }
      case _ => None
    }
  }

  object FunctionDecl {
    def unapply(r : FunctionDeclCommand) : SOption[(Symbol, MESorts, Sort)] = {
      Some((r.symbol_, r.mesorts_, r.sort_))
    }
  }

  object Let {
    def apply(t : Term, bindings : (String, Term)*) : LetTerm = {
      val binds = new ListBindingC
      for ((s, t) <- bindings)
        binds.add(new Binding(new NormalSymbol(s), t))
      new LetTerm(binds, t)
    }
    def unapplySeq(r : LetTerm) : SOption[(Term, Seq[(String, Term)])] = {
      Some((r.term_,
            for (b <- r.listbindingc_) yield {
              val bind = b.asInstanceOf[Binding]
              val s = bind.symbol_.asInstanceOf[NormalSymbol].normalsymbolt_
              (s, bind.term_)
            }))
    }
  }

  object PlainSymbol {
    def apply(s : String) : SymbolRef =
      quoteIdentifier(s) match {
        case `s` =>
          new IdentifierRef(new SymbolIdent(new NormalSymbol(s)))
        case qs =>
          new IdentifierRef(new SymbolIdent(new QuotedSymbol(qs)))
      }
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
    def apply(s : String, indexes : String*) : SymbolRef = {
      val indexList = new ListIndexC
      for (ind <- indexes)
        indexList.add(new Index(ind))
      quoteIdentifier(s) match {
        case `s` =>
          new IdentifierRef(new IndexIdent(new NormalSymbol(s), indexList))
        case qs =>
          new IdentifierRef(new IndexIdent(new QuotedSymbol(qs), indexList))
      }
    }
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

  object AssertCmd {
    def apply(t : Term) = new AssertCommand(t)
    def unapply(cmd : AssertCommand) : scala.Option[Term] = Some(cmd.term_)
  }

    abstract class FindDataVisitor[Data]
           extends FoldVisitor[SOption[Data], Unit] {
    def apply(cmd : Command) : SOption[Data] = cmd.accept(this, ())
    def leaf(arg : Unit) = None
    def combine(x : SOption[Data], y : SOption[Data], arg : Unit) = x orElse y
  }

  object FindSymbolVisitor {
    def apply[Data](cmd : Command)
                   (pred : String => SOption[Data]) : SOption[Data] =
      (new FindSymbolVisitor(pred))(cmd)
  }

  class FindSymbolVisitor[Data](pred : String => SOption[Data])
        extends FindDataVisitor[Data] {
    override def visit(p : NormalSymbol, arg : Unit) : SOption[Data] =
      pred(p.normalsymbolt_)
    override def visit(p : QuotedSymbol, arg : Unit) : SOption[Data] =
      pred(p.quotedsymbolt_.substring(1, p.quotedsymbolt_.size - 1))
  }

  object ContainsSymbolVisitor {
    def apply(cmd : Command)(pred : String => Boolean) : Boolean =
      (new ContainsSymbolVisitor(pred))(cmd)
  }

  class ContainsSymbolVisitor(pred : String => Boolean)
        extends FoldVisitor[Boolean, Unit] {
    def apply(cmd : Command) : Boolean =
      cmd.accept(this, ())

    def leaf(arg : Unit) = false
    def combine(x : Boolean, y : Boolean, arg : Unit) = x || y

    override def visit(p : NormalSymbol, arg : Unit) : Boolean =
      pred(p.normalsymbolt_)
    override def visit(p : QuotedSymbol, arg : Unit) : Boolean =
      pred(p.quotedsymbolt_.substring(1, p.quotedsymbolt_.size - 1))
  }

}
