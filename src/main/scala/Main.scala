import core.CompilerX
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

object Main extends App {

  def plusOne(x: Int): Int = {
    if (x == 5) {
      throw new Exception("ololo")
    }
    x + 1
  }

//  val valDef = Trees.ValDef(Names.termName("_"), Trees.theEmptyTree, Trees.theEmptyTree)
//  println(valDef)
//  System.exit(0)

  System.setProperty("scala.usejavacp", "true")

  implicit def ctx: Context = (new ContextBase).initialCtx

    val compiler = new CompilerX

//    val fileNames = List(
//      "/Users/baskakov/IdeaProjects/dynamiteSSA/src/main/scala/package2.scala"
//    )
    val fileNames = List("/Users/baskakov/IdeaProjects/dynamiteSSA/src/main/resources/Enterprise.scala")

    val run = compiler.newRun
    run.compile(fileNames)
    run.printSummary()

}
