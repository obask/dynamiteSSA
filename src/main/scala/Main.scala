import core.CompilerX
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

object Main extends App {

  System.setProperty("scala.usejavacp", "true")

  implicit def ctx: Context = (new ContextBase).initialCtx

  /** Entry point to the compiler using a custom `Context`.
    *
    *  In most cases, you do not need a custom `Context` and should
    *  instead use one of the other overloads of `process`. However,
    *  the other overloads cannot be overriden, instead you
    *  should override this one which they call internally.
    *
    *  Usage example: [[https://github.com/lampepfl/dotty/tree/master/test/test/OtherEntryPointsTest.scala]]
    *  in method `runCompilerWithContext`
    *
    */

    val compiler = new CompilerX

    val fileNames = List("/Users/baskakov/delme/Enterprise.scala")

    val run = compiler.newRun
    run.compile(fileNames)
    run.printSummary()

}
