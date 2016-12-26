import dotty.tools.dotc.core.{Constants, Names, Types}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.Trees.Untyped
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Types.{NoPrefix, ThisType, TypeRef}
import dotty.tools.dotc.printing.RefinedPrinter
import goast.Tokens.Wrapper
import parser._

import scala.io.Source

object Dotty extends App {

  case class TypeNameX(value: String) extends Types.ValueType {
    override def hash: Int = ???
  }

  case class JavaArrayType(value: Types.Type) extends Types.ValueType {
    override def hash: Int = ???
  }

  case class TypeTreeX[-T >: Untyped](value: Types.Type) extends Trees.Tree

  def handleType(tree: CodeTree): Types.Type = {
    println("handleType: " + tree)

    tree match {
      //      case ABranch("TypeRef", params) =>
      //        val tmp = TypeName(params(1).asInstanceOf[ASymbol].value)
      //        createCaseClass(TypeRef, Seq(handleType(params.head), tmp))

      case ABranch("tn", params) =>
        val nameOfType = params.head.asInstanceOf[ASymbol].value
        TypeNameX(nameOfType)
      case ABranch("t", params) =>
        println("QWEQRTYTRQWEWYUTWGMDSGNSDONGJDSNGKJSNG: " + params)
        handleType(tree)
      //        handleDefault(tree).asInstanceOf[Type]
      case ABranch(cmd, params) =>
        val args = params.map(handleType)
        cmd match {
          case "NoPrefix" => NoPrefix
//          case "ThisType" => createCaseClass(Types.ThisType, args)
          case "JavaArrayType" => JavaArrayType(args.head)
        }
      case _ => handleDefault(tree).asInstanceOf[Types.Type]
    }
  }


  def handleDefault(tree: CodeTree): Any = {
    println(tree)
    tree match {
      case ABranch("t", params) =>
        println("PROCESS TT: " + params)
        val value = params.head.asInstanceOf[ASymbol].value
//        Types.TypeRef(Types.NoPrefix, Names.typeName(value))
        TypeTreeX(TypeNameX(value))
      case _ => ???
    }
  }


  def handleAST(tree: CodeTree): Any = {
    //    println("----")
    //    println("handleAST: " + tree.toString.take(20))
    tree match {
      case ASymbol("Nil") =>
        Nil
      case ASymbol("EmptyTree") =>
        Trees.theEmptyTree
      case ASymbol(info) =>
        println("TermName: " + info)
        Names.termName(info)
      case ABranch("%", params) =>
        params.map(handleAST)
      case ABranch("tn", List()) =>
        Names.typeName("")
      case ABranch("tn", List(ASymbol(ss))) =>
        Names.typeName(ss)
      //      case ABranch("TypeRef", params) =>
      //        handleType(tree)
      case ABranch("TypeTree", params) =>
        TypeTreeX(handleType(params.head))
      case ABranch("Constant", params) =>
        params match {
          case Seq(ANumber(x)) => Constants.Constant(x)
          case Seq(ADouble(x)) => Constants.Constant(x)
          case Seq(ASymbol("Unit")) => Constants.Constant(Constants.UnitTag)
          case ll: List[ASymbol] =>
            val tmp = ll.map(_.value).mkString(" ")
            Constants.Constant(tmp)
        }
      case ABranch(cmd, params) =>
        val args = params.map(handleAST)
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
          case "ValDef" => createCaseClass(ValDef, args): ValDef[Untyped]
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

  def createCaseClass[T, T1](o: T1, args: Seq[Any]): T = {
    println("createCaseClass: " + o.getClass.getName)
    println("args " + args.length.toString + ": " + args.map(_.toString.take(100)).mkString(" |#| "))

    val tmp = args map { _.asInstanceOf[AnyRef] }
//    println(o.getClass.getMethods.toSeq.mkString("\n"))
    o.getClass.getMethods
      .find(x => x.getName == "apply" && !x.isBridge)
      .get.invoke(o, tmp: _*)
      .asInstanceOf[T]
  }


  val arr = Seq(Names.termName("args"), TypeTreeX(TypeNameX("")), theEmptyTree)
  val tmp = arr map { _.asInstanceOf[AnyRef] }

  val res = createCaseClass(Trees.ValDef, arr): ValDef[Untyped]
//    .find(x => x.getName == "apply" && !x.isBridge)
//    .get.invoke(o, tmp: _*)

  println("TMP = " + res)

  val path = "/Users/baskakov/IdeaProjects/dynamiteSSA/src/main/resources/result.sexpr"

  implicit def ctx: Context = (new ContextBase).initialCtx
  println("BEGIN =============================")
  val programFile = Source.fromURL(getClass.getResource("/result.sexpr"))
  val myLispProgram = programFile.getLines().mkString(" ")
  val tokens = Parser.tokenize(myLispProgram)
  println("makeFullAST =============================")
  val tree = Parser.makeFullAST(tokens.toList).head
  println("handleAST =============================")
  val ast = handleAST(tree).asInstanceOf[Trees.Tree[Trees.Untyped]]

  println("TMP =============================")


//  val tree = Trees.Apply(Trees.Ident(Names.termName("ololo")), List())
//
//
//  val tmp = List(Trees.Ident(Names.termName("ololo")), List()).toArray
//
//
////  val qqq = tpd.Apply(tpd.Ident(Names.termName("ololo")), List())
//
//  val res: Trees.Tree[Untyped] = createCaseClass(Trees.Apply, tmp)

  //  println(Trees.Apply.getClass.getMethods.toList.map(_.getName))
//    println(res)

  val printer = new RefinedPrinter(ctx)
  println(printer.toText(ast).mkString(width = 100))

}
