
import ast.IRNode.{ArrayAssign, DefDef, Prototype, ValDef}
import ast.Trees._
import ast._
import hlvm._

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.io.Source


object Main {


//  (Apply (Ident println) (# (Literal (Constant "Hello World"))))

  def handleAST(tree: CodeTree): Any = {
    tree match {
      case ASymbol(info) =>
        println(info)
        TermName(info)
      case ABranch(cmd, params) =>
        println(cmd)
        val args = params.map(handleAST)
        cmd match {
          case "#" => args
          case "Apply" => createCaseClass(Apply, args)
          case "Ident" => createCaseClass(Ident, args)
          case "Literal" => createCaseClass(Literal, args)
          case "Constant" => createCaseClass(Constant, args)
        }
      case _ => ???
    }
  }

  val programFile = Source.fromURL(getClass.getResource("/hello.ir"))

  val myLispProgram = programFile.getLines().mkString(" ")


  def createCaseClass[T, T1](o: T1, args: Seq[Any]): T = {
    println("createCaseClass: " + o.getClass.getName)
    println(args)
    val tmp = args map { _.asInstanceOf[AnyRef] }
    o.getClass.getMethods.find(x => x.getName == "apply" && x.isBridge).get.invoke(o, tmp: _*).asInstanceOf[T]
  }


  case class Qwerty[T](a: Int, b: String) {
    def ttt(x: Any) = {
      x match {
        case a: T => null
        case _ => null
      }
    }
  }

  def main(args: Array[String]) {

    val args = List(TermName("qqqqq"))
    val tmp = args map { _.asInstanceOf[AnyRef] }
    println(Ident.getClass.getMethods.toSeq.mkString("\n"))
    val res0 = Ident.getClass.getMethods.find(x => x.getName == "apply" && x.isBridge)
    val res = res0.get.invoke(Ident, tmp: _*).asInstanceOf[Ident]
    println(res)

    return


    val tokens = Parser.tokenize(myLispProgram)
    val tree = Parser.makeFullAST(tokens.toList).head
    val ast = handleAST(tree)

    println(ast)

    //    IRNode.TheModule.getFunction("main1").dump()
//    println(IRNode.TheModule.getFunction("main1").codegen.mkString("\n"))


//    println(IRNode.TheModule.functions.keys)

  }

}


object Main3 extends App {

//  (DefDef main Nil (# (:: (ValDef args (TypeTree (Thicket Nil)) (Thicket Nil)) Nil)) (TypeTree (Thicket Nil)) (Apply (Ident println) (# (Literal (Constant "...............hello, )(((World!")))))))) (ValDef Enterprise (TypeTree (Thicket Nil)) (Apply (Select (New (TypeTree (Thicket Nil))) <init>) Nil)))

  val tree = Apply( Select( New( TypeTree( Thicket( Nil))), TermName("<init>")), Nil)


  println(tree)

}

