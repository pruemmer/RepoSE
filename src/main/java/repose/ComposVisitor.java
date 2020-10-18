package repose;
import ap.parser.smtlib.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  ap.parser.smtlib.Absyn.ScriptC.Visitor<ap.parser.smtlib.Absyn.ScriptC,A>,
  ap.parser.smtlib.Absyn.Command.Visitor<ap.parser.smtlib.Absyn.Command,A>,
  ap.parser.smtlib.Absyn.OptionC.Visitor<ap.parser.smtlib.Absyn.OptionC,A>,
  ap.parser.smtlib.Absyn.Sort.Visitor<ap.parser.smtlib.Absyn.Sort,A>,
  ap.parser.smtlib.Absyn.MESorts.Visitor<ap.parser.smtlib.Absyn.MESorts,A>,
  ap.parser.smtlib.Absyn.ConstructorDeclC.Visitor<ap.parser.smtlib.Absyn.ConstructorDeclC,A>,
  ap.parser.smtlib.Absyn.SelectorDeclC.Visitor<ap.parser.smtlib.Absyn.SelectorDeclC,A>,
  ap.parser.smtlib.Absyn.PolySortC.Visitor<ap.parser.smtlib.Absyn.PolySortC,A>,
  ap.parser.smtlib.Absyn.MaybeParDataDecl.Visitor<ap.parser.smtlib.Absyn.MaybeParDataDecl,A>,
  ap.parser.smtlib.Absyn.OldDataDeclC.Visitor<ap.parser.smtlib.Absyn.OldDataDeclC,A>,
  ap.parser.smtlib.Absyn.Term.Visitor<ap.parser.smtlib.Absyn.Term,A>,
  ap.parser.smtlib.Absyn.BindingC.Visitor<ap.parser.smtlib.Absyn.BindingC,A>,
  ap.parser.smtlib.Absyn.Quantifier.Visitor<ap.parser.smtlib.Absyn.Quantifier,A>,
  ap.parser.smtlib.Absyn.SymbolRef.Visitor<ap.parser.smtlib.Absyn.SymbolRef,A>,
  ap.parser.smtlib.Absyn.FunSignatureC.Visitor<ap.parser.smtlib.Absyn.FunSignatureC,A>,
  ap.parser.smtlib.Absyn.SortedVariableC.Visitor<ap.parser.smtlib.Absyn.SortedVariableC,A>,
  ap.parser.smtlib.Absyn.ESortedVarC.Visitor<ap.parser.smtlib.Absyn.ESortedVarC,A>,
  ap.parser.smtlib.Absyn.SpecConstant.Visitor<ap.parser.smtlib.Absyn.SpecConstant,A>,
  ap.parser.smtlib.Absyn.MetaConstant.Visitor<ap.parser.smtlib.Absyn.MetaConstant,A>,
  ap.parser.smtlib.Absyn.Identifier.Visitor<ap.parser.smtlib.Absyn.Identifier,A>,
  ap.parser.smtlib.Absyn.IndexC.Visitor<ap.parser.smtlib.Absyn.IndexC,A>,
  ap.parser.smtlib.Absyn.Symbol.Visitor<ap.parser.smtlib.Absyn.Symbol,A>,
  ap.parser.smtlib.Absyn.Annotation.Visitor<ap.parser.smtlib.Absyn.Annotation,A>,
  ap.parser.smtlib.Absyn.AttrParam.Visitor<ap.parser.smtlib.Absyn.AttrParam,A>,
  ap.parser.smtlib.Absyn.SExpr.Visitor<ap.parser.smtlib.Absyn.SExpr,A>
{
/* ScriptC */
    public ScriptC visit(ap.parser.smtlib.Absyn.Script p, A arg)
    {
      ListCommand listcommand_ = new ListCommand();
      for (Command x : p.listcommand_)
      {
        listcommand_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.Script(listcommand_);
    }
/* Command */
    public Command visit(ap.parser.smtlib.Absyn.SetLogicCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SetLogicCommand(symbol_);
    }    public Command visit(ap.parser.smtlib.Absyn.SetOptionCommand p, A arg)
    {
      OptionC optionc_ = p.optionc_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SetOptionCommand(optionc_);
    }    public Command visit(ap.parser.smtlib.Absyn.SetInfoCommand p, A arg)
    {
      Annotation annotation_ = p.annotation_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SetInfoCommand(annotation_);
    }    public Command visit(ap.parser.smtlib.Absyn.SortDeclCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      String numeral_ = p.numeral_;
      return new ap.parser.smtlib.Absyn.SortDeclCommand(symbol_, numeral_);
    }    public Command visit(ap.parser.smtlib.Absyn.SortDefCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListSymbol listsymbol_ = new ListSymbol();
      for (Symbol x : p.listsymbol_)
      {
        listsymbol_.add(x.accept(this,arg));
      }
      Sort sort_ = p.sort_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SortDefCommand(symbol_, listsymbol_, sort_);
    }    public Command visit(ap.parser.smtlib.Absyn.DataDeclsCommand p, A arg)
    {
      ListPolySortC listpolysortc_ = new ListPolySortC();
      for (PolySortC x : p.listpolysortc_)
      {
        listpolysortc_.add(x.accept(this,arg));
      }
      ListMaybeParDataDecl listmaybepardatadecl_ = new ListMaybeParDataDecl();
      for (MaybeParDataDecl x : p.listmaybepardatadecl_)
      {
        listmaybepardatadecl_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.DataDeclsCommand(listpolysortc_, listmaybepardatadecl_);
    }    public Command visit(ap.parser.smtlib.Absyn.DataDeclsOldCommand p, A arg)
    {
      ListSymbol listsymbol_ = new ListSymbol();
      for (Symbol x : p.listsymbol_)
      {
        listsymbol_.add(x.accept(this,arg));
      }
      ListOldDataDeclC listolddatadeclc_ = new ListOldDataDeclC();
      for (OldDataDeclC x : p.listolddatadeclc_)
      {
        listolddatadeclc_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.DataDeclsOldCommand(listsymbol_, listolddatadeclc_);
    }    public Command visit(ap.parser.smtlib.Absyn.DataDeclCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListConstructorDeclC listconstructordeclc_ = new ListConstructorDeclC();
      for (ConstructorDeclC x : p.listconstructordeclc_)
      {
        listconstructordeclc_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.DataDeclCommand(symbol_, listconstructordeclc_);
    }    public Command visit(ap.parser.smtlib.Absyn.FunctionDeclCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      MESorts mesorts_ = p.mesorts_.accept(this, arg);
      Sort sort_ = p.sort_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.FunctionDeclCommand(symbol_, mesorts_, sort_);
    }    public Command visit(ap.parser.smtlib.Absyn.ConstDeclCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      Sort sort_ = p.sort_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.ConstDeclCommand(symbol_, sort_);
    }    public Command visit(ap.parser.smtlib.Absyn.FunctionDefCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListESortedVarC listesortedvarc_ = new ListESortedVarC();
      for (ESortedVarC x : p.listesortedvarc_)
      {
        listesortedvarc_.add(x.accept(this,arg));
      }
      Sort sort_ = p.sort_.accept(this, arg);
      Term term_ = p.term_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.FunctionDefCommand(symbol_, listesortedvarc_, sort_, term_);
    }    public Command visit(ap.parser.smtlib.Absyn.ConstDefCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      Sort sort_ = p.sort_.accept(this, arg);
      Term term_ = p.term_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.ConstDefCommand(symbol_, sort_, term_);
    }    public Command visit(ap.parser.smtlib.Absyn.RecFunctionDefCommand p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListESortedVarC listesortedvarc_ = new ListESortedVarC();
      for (ESortedVarC x : p.listesortedvarc_)
      {
        listesortedvarc_.add(x.accept(this,arg));
      }
      Sort sort_ = p.sort_.accept(this, arg);
      Term term_ = p.term_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.RecFunctionDefCommand(symbol_, listesortedvarc_, sort_, term_);
    }    public Command visit(ap.parser.smtlib.Absyn.RecFunctionDefsCommand p, A arg)
    {
      ListFunSignatureC listfunsignaturec_ = new ListFunSignatureC();
      for (FunSignatureC x : p.listfunsignaturec_)
      {
        listfunsignaturec_.add(x.accept(this,arg));
      }
      ListTerm listterm_ = new ListTerm();
      for (Term x : p.listterm_)
      {
        listterm_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.RecFunctionDefsCommand(listfunsignaturec_, listterm_);
    }    public Command visit(ap.parser.smtlib.Absyn.PushCommand p, A arg)
    {
      String numeral_ = p.numeral_;
      return new ap.parser.smtlib.Absyn.PushCommand(numeral_);
    }    public Command visit(ap.parser.smtlib.Absyn.PopCommand p, A arg)
    {
      String numeral_ = p.numeral_;
      return new ap.parser.smtlib.Absyn.PopCommand(numeral_);
    }    public Command visit(ap.parser.smtlib.Absyn.AssertCommand p, A arg)
    {
      Term term_ = p.term_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.AssertCommand(term_);
    }    public Command visit(ap.parser.smtlib.Absyn.CheckSatCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.CheckSatCommand();
    }    public Command visit(ap.parser.smtlib.Absyn.GetAssertionsCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.GetAssertionsCommand();
    }    public Command visit(ap.parser.smtlib.Absyn.GetValueCommand p, A arg)
    {
      ListTerm listterm_ = new ListTerm();
      for (Term x : p.listterm_)
      {
        listterm_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.GetValueCommand(listterm_);
    }    public Command visit(ap.parser.smtlib.Absyn.GetProofCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.GetProofCommand();
    }    public Command visit(ap.parser.smtlib.Absyn.GetUnsatCoreCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.GetUnsatCoreCommand();
    }    public Command visit(ap.parser.smtlib.Absyn.GetAssignmentCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.GetAssignmentCommand();
    }    public Command visit(ap.parser.smtlib.Absyn.GetModelCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.GetModelCommand();
    }    public Command visit(ap.parser.smtlib.Absyn.GetInterpolantsCommand p, A arg)
    {
      ListSExpr listsexpr_ = new ListSExpr();
      for (SExpr x : p.listsexpr_)
      {
        listsexpr_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.GetInterpolantsCommand(listsexpr_);
    }    public Command visit(ap.parser.smtlib.Absyn.GetInfoCommand p, A arg)
    {
      String annotattribute_ = p.annotattribute_;
      return new ap.parser.smtlib.Absyn.GetInfoCommand(annotattribute_);
    }    public Command visit(ap.parser.smtlib.Absyn.GetOptionCommand p, A arg)
    {
      String annotattribute_ = p.annotattribute_;
      return new ap.parser.smtlib.Absyn.GetOptionCommand(annotattribute_);
    }    public Command visit(ap.parser.smtlib.Absyn.EchoCommand p, A arg)
    {
      String smtstring_ = p.smtstring_;
      return new ap.parser.smtlib.Absyn.EchoCommand(smtstring_);
    }    public Command visit(ap.parser.smtlib.Absyn.ResetCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.ResetCommand();
    }    public Command visit(ap.parser.smtlib.Absyn.ExitCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.ExitCommand();
    }    public Command visit(ap.parser.smtlib.Absyn.IgnoreCommand p, A arg)
    {
      Term term_ = p.term_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.IgnoreCommand(term_);
    }    public Command visit(ap.parser.smtlib.Absyn.EmptyCommand p, A arg)
    {
      return new ap.parser.smtlib.Absyn.EmptyCommand();
    }
/* OptionC */
    public OptionC visit(ap.parser.smtlib.Absyn.Option p, A arg)
    {
      Annotation annotation_ = p.annotation_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.Option(annotation_);
    }
/* Sort */
    public Sort visit(ap.parser.smtlib.Absyn.IdentSort p, A arg)
    {
      Identifier identifier_ = p.identifier_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.IdentSort(identifier_);
    }    public Sort visit(ap.parser.smtlib.Absyn.CompositeSort p, A arg)
    {
      Identifier identifier_ = p.identifier_.accept(this, arg);
      ListSort listsort_ = new ListSort();
      for (Sort x : p.listsort_)
      {
        listsort_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.CompositeSort(identifier_, listsort_);
    }
/* MESorts */
    public MESorts visit(ap.parser.smtlib.Absyn.SomeSorts p, A arg)
    {
      ListSort listsort_ = new ListSort();
      for (Sort x : p.listsort_)
      {
        listsort_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.SomeSorts(listsort_);
    }    public MESorts visit(ap.parser.smtlib.Absyn.NoSorts p, A arg)
    {
      return new ap.parser.smtlib.Absyn.NoSorts();
    }
/* ConstructorDeclC */
    public ConstructorDeclC visit(ap.parser.smtlib.Absyn.NullConstructorDecl p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.NullConstructorDecl(symbol_);
    }    public ConstructorDeclC visit(ap.parser.smtlib.Absyn.ConstructorDecl p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListSelectorDeclC listselectordeclc_ = new ListSelectorDeclC();
      for (SelectorDeclC x : p.listselectordeclc_)
      {
        listselectordeclc_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.ConstructorDecl(symbol_, listselectordeclc_);
    }
/* SelectorDeclC */
    public SelectorDeclC visit(ap.parser.smtlib.Absyn.SelectorDecl p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      Sort sort_ = p.sort_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SelectorDecl(symbol_, sort_);
    }
/* PolySortC */
    public PolySortC visit(ap.parser.smtlib.Absyn.PolySort p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      String numeral_ = p.numeral_;
      return new ap.parser.smtlib.Absyn.PolySort(symbol_, numeral_);
    }
/* MaybeParDataDecl */
    public MaybeParDataDecl visit(ap.parser.smtlib.Absyn.MonoDataDecl p, A arg)
    {
      ListConstructorDeclC listconstructordeclc_ = new ListConstructorDeclC();
      for (ConstructorDeclC x : p.listconstructordeclc_)
      {
        listconstructordeclc_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.MonoDataDecl(listconstructordeclc_);
    }    public MaybeParDataDecl visit(ap.parser.smtlib.Absyn.ParDataDecl p, A arg)
    {
      ListSymbol listsymbol_ = new ListSymbol();
      for (Symbol x : p.listsymbol_)
      {
        listsymbol_.add(x.accept(this,arg));
      }
      ListConstructorDeclC listconstructordeclc_ = new ListConstructorDeclC();
      for (ConstructorDeclC x : p.listconstructordeclc_)
      {
        listconstructordeclc_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.ParDataDecl(listsymbol_, listconstructordeclc_);
    }
/* OldDataDeclC */
    public OldDataDeclC visit(ap.parser.smtlib.Absyn.OldDataDecl p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListConstructorDeclC listconstructordeclc_ = new ListConstructorDeclC();
      for (ConstructorDeclC x : p.listconstructordeclc_)
      {
        listconstructordeclc_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.OldDataDecl(symbol_, listconstructordeclc_);
    }
/* Term */
    public Term visit(ap.parser.smtlib.Absyn.ConstantTerm p, A arg)
    {
      SpecConstant specconstant_ = p.specconstant_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.ConstantTerm(specconstant_);
    }    public Term visit(ap.parser.smtlib.Absyn.NullaryTerm p, A arg)
    {
      SymbolRef symbolref_ = p.symbolref_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.NullaryTerm(symbolref_);
    }    public Term visit(ap.parser.smtlib.Absyn.FunctionTerm p, A arg)
    {
      SymbolRef symbolref_ = p.symbolref_.accept(this, arg);
      ListTerm listterm_ = new ListTerm();
      for (Term x : p.listterm_)
      {
        listterm_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.FunctionTerm(symbolref_, listterm_);
    }    public Term visit(ap.parser.smtlib.Absyn.LetTerm p, A arg)
    {
      ListBindingC listbindingc_ = new ListBindingC();
      for (BindingC x : p.listbindingc_)
      {
        listbindingc_.add(x.accept(this,arg));
      }
      Term term_ = p.term_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.LetTerm(listbindingc_, term_);
    }    public Term visit(ap.parser.smtlib.Absyn.QuantifierTerm p, A arg)
    {
      Quantifier quantifier_ = p.quantifier_.accept(this, arg);
      ListSortedVariableC listsortedvariablec_ = new ListSortedVariableC();
      for (SortedVariableC x : p.listsortedvariablec_)
      {
        listsortedvariablec_.add(x.accept(this,arg));
      }
      Term term_ = p.term_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.QuantifierTerm(quantifier_, listsortedvariablec_, term_);
    }    public Term visit(ap.parser.smtlib.Absyn.AnnotationTerm p, A arg)
    {
      Term term_ = p.term_.accept(this, arg);
      ListAnnotation listannotation_ = new ListAnnotation();
      for (Annotation x : p.listannotation_)
      {
        listannotation_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.AnnotationTerm(term_, listannotation_);
    }
/* BindingC */
    public BindingC visit(ap.parser.smtlib.Absyn.Binding p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      Term term_ = p.term_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.Binding(symbol_, term_);
    }
/* Quantifier */
    public Quantifier visit(ap.parser.smtlib.Absyn.AllQuantifier p, A arg)
    {
      return new ap.parser.smtlib.Absyn.AllQuantifier();
    }    public Quantifier visit(ap.parser.smtlib.Absyn.ExQuantifier p, A arg)
    {
      return new ap.parser.smtlib.Absyn.ExQuantifier();
    }    public Quantifier visit(ap.parser.smtlib.Absyn.EpsQuantifier p, A arg)
    {
      return new ap.parser.smtlib.Absyn.EpsQuantifier();
    }
/* SymbolRef */
    public SymbolRef visit(ap.parser.smtlib.Absyn.IdentifierRef p, A arg)
    {
      Identifier identifier_ = p.identifier_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.IdentifierRef(identifier_);
    }    public SymbolRef visit(ap.parser.smtlib.Absyn.CastIdentifierRef p, A arg)
    {
      Identifier identifier_ = p.identifier_.accept(this, arg);
      Sort sort_ = p.sort_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.CastIdentifierRef(identifier_, sort_);
    }
/* FunSignatureC */
    public FunSignatureC visit(ap.parser.smtlib.Absyn.FunSignature p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListESortedVarC listesortedvarc_ = new ListESortedVarC();
      for (ESortedVarC x : p.listesortedvarc_)
      {
        listesortedvarc_.add(x.accept(this,arg));
      }
      Sort sort_ = p.sort_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.FunSignature(symbol_, listesortedvarc_, sort_);
    }
/* SortedVariableC */
    public SortedVariableC visit(ap.parser.smtlib.Absyn.SortedVariable p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      Sort sort_ = p.sort_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SortedVariable(symbol_, sort_);
    }
/* ESortedVarC */
    public ESortedVarC visit(ap.parser.smtlib.Absyn.ESortedVar p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      Sort sort_ = p.sort_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.ESortedVar(symbol_, sort_);
    }
/* SpecConstant */
    public SpecConstant visit(ap.parser.smtlib.Absyn.NumConstant p, A arg)
    {
      String numeral_ = p.numeral_;
      return new ap.parser.smtlib.Absyn.NumConstant(numeral_);
    }    public SpecConstant visit(ap.parser.smtlib.Absyn.RatConstant p, A arg)
    {
      String rational_ = p.rational_;
      return new ap.parser.smtlib.Absyn.RatConstant(rational_);
    }    public SpecConstant visit(ap.parser.smtlib.Absyn.HexConstant p, A arg)
    {
      String hexadecimal_ = p.hexadecimal_;
      return new ap.parser.smtlib.Absyn.HexConstant(hexadecimal_);
    }    public SpecConstant visit(ap.parser.smtlib.Absyn.BinConstant p, A arg)
    {
      String binary_ = p.binary_;
      return new ap.parser.smtlib.Absyn.BinConstant(binary_);
    }    public SpecConstant visit(ap.parser.smtlib.Absyn.StringConstant p, A arg)
    {
      String smtstring_ = p.smtstring_;
      return new ap.parser.smtlib.Absyn.StringConstant(smtstring_);
    }
/* MetaConstant */
    public MetaConstant visit(ap.parser.smtlib.Absyn.NumMetaConstant p, A arg)
    {
      return new ap.parser.smtlib.Absyn.NumMetaConstant();
    }    public MetaConstant visit(ap.parser.smtlib.Absyn.RatMetaConstant p, A arg)
    {
      return new ap.parser.smtlib.Absyn.RatMetaConstant();
    }    public MetaConstant visit(ap.parser.smtlib.Absyn.HexMetaConstant p, A arg)
    {
      return new ap.parser.smtlib.Absyn.HexMetaConstant();
    }    public MetaConstant visit(ap.parser.smtlib.Absyn.BinMetaConstant p, A arg)
    {
      return new ap.parser.smtlib.Absyn.BinMetaConstant();
    }    public MetaConstant visit(ap.parser.smtlib.Absyn.StringMetaConstant p, A arg)
    {
      return new ap.parser.smtlib.Absyn.StringMetaConstant();
    }
/* Identifier */
    public Identifier visit(ap.parser.smtlib.Absyn.SymbolIdent p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SymbolIdent(symbol_);
    }    public Identifier visit(ap.parser.smtlib.Absyn.IndexIdent p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListIndexC listindexc_ = new ListIndexC();
      for (IndexC x : p.listindexc_)
      {
        listindexc_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.IndexIdent(symbol_, listindexc_);
    }
/* IndexC */
    public IndexC visit(ap.parser.smtlib.Absyn.Index p, A arg)
    {
      String numeral_ = p.numeral_;
      return new ap.parser.smtlib.Absyn.Index(numeral_);
    }
/* Symbol */
    public Symbol visit(ap.parser.smtlib.Absyn.NormalSymbol p, A arg)
    {
      String normalsymbolt_ = p.normalsymbolt_;
      return new ap.parser.smtlib.Absyn.NormalSymbol(normalsymbolt_);
    }    public Symbol visit(ap.parser.smtlib.Absyn.QuotedSymbol p, A arg)
    {
      String quotedsymbolt_ = p.quotedsymbolt_;
      return new ap.parser.smtlib.Absyn.QuotedSymbol(quotedsymbolt_);
    }
/* Annotation */
    public Annotation visit(ap.parser.smtlib.Absyn.AttrAnnotation p, A arg)
    {
      String annotattribute_ = p.annotattribute_;
      AttrParam attrparam_ = p.attrparam_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.AttrAnnotation(annotattribute_, attrparam_);
    }
/* AttrParam */
    public AttrParam visit(ap.parser.smtlib.Absyn.SomeAttrParam p, A arg)
    {
      SExpr sexpr_ = p.sexpr_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SomeAttrParam(sexpr_);
    }    public AttrParam visit(ap.parser.smtlib.Absyn.NoAttrParam p, A arg)
    {
      return new ap.parser.smtlib.Absyn.NoAttrParam();
    }
/* SExpr */
    public SExpr visit(ap.parser.smtlib.Absyn.ConstantSExpr p, A arg)
    {
      SpecConstant specconstant_ = p.specconstant_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.ConstantSExpr(specconstant_);
    }    public SExpr visit(ap.parser.smtlib.Absyn.SymbolSExpr p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      return new ap.parser.smtlib.Absyn.SymbolSExpr(symbol_);
    }    public SExpr visit(ap.parser.smtlib.Absyn.ParenSExpr p, A arg)
    {
      ListSExpr listsexpr_ = new ListSExpr();
      for (SExpr x : p.listsexpr_)
      {
        listsexpr_.add(x.accept(this,arg));
      }
      return new ap.parser.smtlib.Absyn.ParenSExpr(listsexpr_);
    }
}
