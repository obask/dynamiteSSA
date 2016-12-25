
import java.io.{StringReader, StringWriter}

import ast.Trees._
import ast._
import clojure.java.api.Clojure
import core.Types._
import parser._

import scala.io.Source


object Main {


// (DefDef
//  <init>
//    Nil
//    (# Nil)
//    (TypeTree (TypeRef (ThisType (TypeRef (NoPrefix) scala)) Unit))
//    (Block
//    (% (Apply (Select (Super (This Enterprise$) Nil) <init>) Nil))
//    (Literal (Constant Unit))))



  def handleType(tree: CodeTree): Type = {
    println("handleType: " + tree)

    tree match {
//      case ABranch("TypeRef", params) =>
//        val tmp = TypeName(params(1).asInstanceOf[ASymbol].value)
//        createCaseClass(TypeRef, Seq(handleType(params.head), tmp))

      case ABranch("tn", params) =>
        val nameOfType = params.head.asInstanceOf[ASymbol].value
        TypeRef(null, TypeName(nameOfType))
      case ABranch("t", params) =>
        handleType(tree)
//        handleDefault(tree).asInstanceOf[Type]
      case ABranch(cmd, params) =>
        val args = params.map(handleType)
        cmd match {
          case "NoPrefix" => NoPrefix
          case "ThisType" => createCaseClass(ThisType, args)
          case "JavaArrayType" => createCaseClass(JavaArrayType, args)
        }
      case _ => handleDefault(tree).asInstanceOf[Type]
    }
  }


  def handleDefault(tree: CodeTree): Any = {
    println(tree)
    tree match {
      case ABranch("t", params) =>
          TypeTree(TypeName(params.head.asInstanceOf[ASymbol].value))
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
        theEmptyTree
      case ASymbol(info) =>
        println("TermName: " + info)
        TermName(info)
      case ABranch("%", params) =>
        params.map(handleAST)
      case ABranch("tn", List()) =>
        TypeName("")
      case ABranch("tn", List(ASymbol(ss))) =>
        TypeName(ss)
//      case ABranch("TypeRef", params) =>
//        handleType(tree)
      case ABranch("TypeTree", params) =>
        TypeTree(handleType(params.head))
      case ABranch("Constant", params) =>
        params match {
          case Seq(ANumber(x)) => Constant(x)
          case Seq(ADouble(x)) => Constant(x)
          case Seq(ASymbol("Unit")) => Constant(Unit)
          case ll: List[ASymbol] =>
            val tmp = ll.map(_.value).mkString(" ")
            Constant(tmp)
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
          case "JavaSeqLiteral" => createCaseClass(JavaSeqLiteral, args)
          case "SingletonTypeTree" => createCaseClass(SingletonTypeTree, args)
          case "SelectFromTypeTree" => createCaseClass(SelectFromTypeTree, args)
          case "AndTypeTree" => createCaseClass(AndTypeTree, args)
          case "OrTypeTree" => createCaseClass(OrTypeTree, args)
          case "RefinedTypeTree" => createCaseClass(RefinedTypeTree, args)
          case "AppliedTypeTree" => createCaseClass(AppliedTypeTree, args)
          case "TypeLambdaTree" => createCaseClass(TypeLambdaTree, args)
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

  val programFile = Source.fromURL(getClass.getResource("/result.sexpr"))

  val myLispProgram = programFile.getLines().mkString(" ")


  def createCaseClass[T, T1](o: T1, args: Seq[Any]): T = {
    println("createCaseClass: " + o.getClass.getName)
    println("args " + args.length.toString + ": " + args.map(_.toString.take(100)).mkString(" |#| "))
    val tmp = args map { _.asInstanceOf[AnyRef] }
//    println(Ident.getClass.getMethods.toSeq.mkString("\n"))
    o.getClass.getMethods
      .find(x => x.getName == "apply" && x.isBridge)
      .get.invoke(o, tmp: _*)
      .asInstanceOf[T]
  }

  def main(args: Array[String]) {
    println("BEGIN =============================")


    val tokens = Parser.tokenize(myLispProgram)
    println("makeFullAST =============================")
    val tree = Parser.makeFullAST(tokens.toList).head
    println("handleAST =============================")
    val ast = handleAST(tree)

    println("TMP =============================")


//    ast.Printer


    //    println(ast.toString.take(100))

    println("AST =============================")

    val source = SExprPrinter.toSource(ast)
    println(source)

    println("PPrint =============================")

//    val eval = Clojure.`var`("clojure.core", "eval")
//    val result = eval.invoke(Clojure.read("(pprint {:a 1 :b 2} :stream nil)"))
    val pw = new StringWriter()

    Clojure.`var`("clojure.pprint", "pprint")
    val result = clojure.lang.Compiler.load(new StringReader(
      "(require 'clojure.pprint)" +
      "(binding [clojure.pprint/*print-right-margin* 100]" +
        "(with-out-str (clojure.pprint/pprint '" + source + ")))"
    ))

    //    println(result)

    println("END =============================")

    //    IRNode.TheModule.getFunction("main1").dump()
//    println(IRNode.TheModule.getFunction("main1").codegen.mkString("\n"))


//    println(IRNode.TheModule.functions.keys)

  }

}
//
//
//object Main3 extends App {
//
////  (DefDef main Nil (# (:: (ValDef args (TypeTree (Thicket Nil)) (Thicket Nil)) Nil)) (TypeTree (Thicket Nil)) (Apply (Ident println) (# (Literal (Constant "...............hello, )(((World!")))))))) (ValDef Enterprise (TypeTree (Thicket Nil)) (Apply (Select (New (TypeTree (Thicket Nil))) <init>) Nil)))
//
//  val tree = Apply( Select( New( TypeTree( Thicket( Nil))), TermName("<init>")), Nil)
//
//
//  println(tree)
//
//}
//
