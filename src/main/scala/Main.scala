
import ast._
import hlvm._

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.io.Source


object Main {

  val programFile = Source.fromURL(getClass.getResource("/code.ir"))

  val myLispProgram = programFile.getLines().mkString("\n")

  def main(args: Array[String]) {
    val tokens = Parser.tokenize(myLispProgram)
    val tree = Parser.makeFullAST(tokens.toList)
    for (t1 <- tree) {
      println("PROCEED: " + t1.asInstanceOf[ABranch].cmd)
      IRNode.handleAST(t1)
    }

    //    IRNode.TheModule.getFunction("main1").dump()
    println(IRNode.TheModule.getFunction("main1").codegen.mkString("\n"))
//    println(IRNode.TheModule.functions.keys)

  }

}


