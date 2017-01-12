package scalast

import core.ADT
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.{Constants, Names}
import sexpr._
import core.createCaseClass

object DReader {

  def handleDottyAST(tree: CodeTree): Any = {
    //    println("----")
    //    println("handleAST: " + tree.toString.take(20))
    tree match {
      case ASymbol("Nil") =>
        Nil
      case ASymbol("EmptyTree") =>
        Trees.theEmptyTree
      case ASymbol("EmptyValDef") =>
        new Trees.EmptyValDef[Trees.Untyped]
      case ASymbol(info) =>
//        println("TermName: " + info)
        Names.termName(info)
      case ABranch("%", params) =>
        params.map(handleDottyAST)
      case ABranch("tn", List()) =>
        Names.typeName("")
      case ABranch("tn", List(ASymbol(ss))) =>
        Names.typeName(ss)
      //      case ABranch("TypeRef", params) =>
      //        handleType(tree)
      case ABranch("TypeTree", params) =>
        ADT.TypeTreeX(handleType(params.head))
      case ABranch("Constant", params) =>
        params match {
          case Seq(ANumber(x)) => Constants.Constant(x)
          case Seq(ADouble(x)) => Constants.Constant(x)
          case Seq(ASymbol("Unit")) => Constants.Constant(())
          case ll: List[ASymbol] =>
            val tmp = ll.map(_.value).mkString(" ")
            Constants.Constant(tmp.substring(1, tmp.length - 1))
        }
      case ABranch(cmd, params) =>
        val args = params.map(handleDottyAST)
        cmd match {
          case "Ident" => createCaseClass(Ident, args)
          case "Select" => createCaseClass(Select, args)
          case "This" => createCaseClass(This, args)
          case "Super" => createCaseClass(Super, args)
          case "Apply" => createCaseClass(Apply, args)
          case "TypeApply" => createCaseClass(TypeApply, args)
          case "Literal" => createCaseClass(Literal, args)
          case "New" => createCaseClass(New, args)
          case "Pair" => createCaseClass(Pair, args)
          case "Typed" => createCaseClass(Typed, args)
          case "NamedArg" => createCaseClass(NamedArg, args)
          case "Assign" => createCaseClass(Assign, args)
          case "Block" => createCaseClass(Block, args)
          case "If" => createCaseClass(If, args)
          case "Closure" => createCaseClass(Closure, args)
          case "Match" => createCaseClass(Match, args)
          case "CaseDef" => createCaseClass(CaseDef, args)
          case "Return" => createCaseClass(Return, args)
          case "Try" => createCaseClass(Try, args)
          case "SeqLiteral" => createCaseClass(SeqLiteral, args)
          //          case "JavaSeqLiteral" => createCaseClass(JavaSeqLiteral, args)
          case "SingletonTypeTree" => createCaseClass(SingletonTypeTree, args)
          //          case "SelectFromTypeTree" => createCaseClass(SelectFromTypeTree, args)
          case "AndTypeTree" => createCaseClass(AndTypeTree, args)
          case "OrTypeTree" => createCaseClass(OrTypeTree, args)
          case "RefinedTypeTree" => createCaseClass(RefinedTypeTree, args)
          case "AppliedTypeTree" => createCaseClass(AppliedTypeTree, args)
          //          case "TypeLambdaTree" => createCaseClass(TypeLambdaTree, args)
          case "ByNameTypeTree" => createCaseClass(ByNameTypeTree, args)
          case "TypeBoundsTree" => createCaseClass(TypeBoundsTree, args)
          case "Bind" => createCaseClass(Bind, args)
          case "Alternative" => createCaseClass(Alternative, args)
          case "UnApply" => createCaseClass(UnApply, args)
          case "ValDef" => createCaseClass(ValDef, args)
          case "DefDef" => createCaseClass(DefDef, args)
          case "TypeDef" => createCaseClass(TypeDef, args)
          case "Template" => createCaseClass(Template, args)
          case "Import" => createCaseClass(Import, args)
          case "PackageDef" => createCaseClass(PackageDef, args)
          case "Annotated" => createCaseClass(Annotated, args)
          case "Thicket" => createCaseClass(Thicket, args)
          case _ => handleDefault(tree)
        }
      case _ => handleDefault(tree)
    }
  }


  private def handleType(tree: CodeTree): ADT.TypeX = {
    println("handleType: " + tree)

    tree match {
      //      case ABranch("TypeRef", params) =>
      //        val tmp = TypeName(params(1).asInstanceOf[ASymbol].value)
      //        createCaseClass(TypeRef, Seq(handleType(params.head), tmp))

      case ABranch("tn", params) =>
        val nameOfType = params.head.asInstanceOf[ASymbol].value
        ADT.TypeNameX(nameOfType)
      case ABranch("t", params) =>
        println("QWEQRTYTRQWEWYUTWGMDSGNSDONGJDSNGKJSNG: " + params)
        handleType(tree)
      //        handleDefault(tree).asInstanceOf[Type]
      case ABranch(cmd, params) =>
        val args = params.map(handleType)
        cmd match {
          case "NoPrefix" => ADT.NoPrefix
          //          case "ThisType" => createCaseClass(Types.ThisType, args)
          case "JavaArrayType" => ADT.JavaArrayTypeX(args.head)
        }
      case _ => handleDefault(tree).asInstanceOf[ADT.TypeX]
    }
  }

  private def handleDefault(tree: CodeTree): Any = {
//    println(tree)
    tree match {
      case ABranch("t", params) =>
//        println("PROCESS TT: " + params)
        val value = params.head.asInstanceOf[ASymbol].value
        //        Types.TypeRef(Types.NoPrefix, Names.typeName(value))
        ADT.TypeTreeX(ADT.TypeNameX(value))
      case _ => ???
    }
  }



}
