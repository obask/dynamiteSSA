import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.printing.RefinedPrinter
import sexpr._

import scala.io.Source
import scalast.{DPrinter, DReader}

object Dotty extends App {

  implicit def ctx: Context = (new ContextBase).initialCtx

  val path = "/Users/baskakov/IdeaProjects/dynamiteSSA/src/main/resources/result.sexpr"



  def main() {
    implicit def ctx: Context = (new ContextBase).initialCtx

    println("== BEGIN ==")
    val programFile = Source.fromURL(getClass.getResource("/result.sexpr"))
    val myLispProgram = programFile.getLines().mkString(" ")
    val tokens = Parser.tokenize(myLispProgram)
    println("makeFullAST =============================")
    val tree = Parser.makeFullAST(tokens.toList).head
    println("handleAST =============================")
    val ast = DReader.handleDottyAST(tree).asInstanceOf[Trees.Tree[Trees.Untyped]]

    println("== TRANSFORM ==")

    val res = transform.def2Golang(ast.asInstanceOf[untpd.DefDef])

    println("TMP =============================")

    val printer = new RefinedPrinter(ctx)
    println(printer.toText(res).mkString(width = 100))

    println("AST =============================")

    val source = DPrinter.toSource(ast)
    println(source)

    println("== END ==")
  }

  main()

}
