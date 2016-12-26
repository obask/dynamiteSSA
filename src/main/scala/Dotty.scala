import java.io.{StringReader, StringWriter}

import clojure.java.api.Clojure
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import parser._

import scala.io.Source
import scalast.{DPrinter, DReader}

object Dotty extends App {

  val path = "/Users/baskakov/IdeaProjects/dynamiteSSA/src/main/resources/result.sexpr"

  def main() {
    implicit def ctx: Context = (new ContextBase).initialCtx

    println("BEGIN =============================")
    val programFile = Source.fromURL(getClass.getResource("/result.sexpr"))
    val myLispProgram = programFile.getLines().mkString(" ")
    val tokens = Parser.tokenize(myLispProgram)
    println("makeFullAST =============================")
    val tree = Parser.makeFullAST(tokens.toList).head
    println("handleAST =============================")
    val ast = DReader.handleDottyAST(tree).asInstanceOf[Trees.Tree[Trees.Untyped]]

    println("TMP =============================")

    //  val printer = new RefinedPrinter(ctx)
    //  println(printer.toText(ast).mkString(width = 100))

    println("AST =============================")

    val source = DPrinter.toSource(ast)
    println(source)

  }


  def legacy(args: Array[String]) {
    println("BEGIN =============================")

    val programFile = Source.fromURL(getClass.getResource("/result.sexpr"))
    val myLispProgram = programFile.getLines().mkString(" ")


    val tokens = Parser.tokenize(myLispProgram)
    println("makeFullAST =============================")
    val tree = Parser.makeFullAST(tokens.toList).head
    println("handleAST =============================")
    val ast = DReader.handleDottyAST(tree)

    println("TMP =============================")


    //    ast.Printer


    //    println(ast.toString.take(100))

    println("AST =============================")

    val source = DPrinter.toSource(ast)
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
