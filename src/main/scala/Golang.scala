
import java.io.{StringReader, StringWriter}

import clojure.java.api.Clojure
import goast.{GoPrinter, GoReader}
import parser._

import scala.io.{BufferedSource, Source}


object Golang {


// (DefDef
//  <init>
//    Nil
//    (# Nil)
//    (TypeTree (TypeRef (ThisType (TypeRef (NoPrefix) scala)) Unit))
//    (Block
//    (% (Apply (Select (Super (This Enterprise$) Nil) <init>) Nil))
//    (Literal (Constant Unit))))



  val programFile: BufferedSource = Source.fromURL(getClass.getResource("/code.clj"))

  val myLispProgram: String = programFile.getLines().mkString(" ")



  def main(args: Array[String]) {
    println("BEGIN =============================")

//    val q1 = List(Ident("Pos"))
//    val q2 = FuncType(FieldList(List()),FieldList(List()))
//    val q3 = Nodes.NilNode
//    val res = Nodes.Field(q1,q2,q3)
//    println(res)
//    return

    val tokens = Parser.tokenize(myLispProgram)
    println("makeFullAST =============================")
    val tree = Parser.makeFullAST(tokens.toList).head
    println("handleAST =============================")
    val ast = GoReader.handleGoAST(tree)

    println("TMP =============================")

    println(ast.toString.take(100))

    println("AST =============================")

    val source = GoPrinter.toSource(ast)
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
    println(result)

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
