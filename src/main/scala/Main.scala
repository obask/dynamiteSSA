
import ast.IRNode.{ArrayAssign, DefDef, Prototype, ValDef}
import ast.Trees._
import ast._
import hlvm._

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.io.Source


object Main {



  def handleAST(tree: CodeTree): Unit = {
    assert(tree.isInstanceOf[ABranch])
    tree match {
      case ABranch("PackageDef", Seq(name: ASymbol, args: ABranch, body: CodeTree)) =>
        DefDef(name, args, body).handleIR()

      case ABranch("ValDef", List(varName: ASymbol, valueX: CodeTree)) =>
        ValDef(varName, valueX).handleIR()

      case ABranch("ArrayAssign", List(varName: ASymbol, posX: CodeTree, valueX: CodeTree)) =>
        ArrayAssign(varName, posX, valueX).handleIR()

      case ABranch(cmd: String, params) =>
        cmd match {
          case "Prototype" =>
            val name = params.head.asInstanceOf[ASymbol].value
            assert("#" == params(1).asInstanceOf[ABranch].cmd)
            val args = params(1).asInstanceOf[ABranch].params
            assert("->" == params(2).asInstanceOf[ASymbol].value)
            val ret = params(3)
            Prototype(name, args, ret).handleIR()
          case "TypeDef" => ???
          case "TypeFFI" => ???
          case "ArrayAssign" => ???
          case _ =>
            println("OLOLO")
        }

    }
  }

  val programFile = Source.fromURL(getClass.getResource("/hello.ir"))

  val myLispProgram = programFile.getLines().mkString(" ")


  def createCaseClass[T](o: T, args: Seq[Any]) = {
    val tmp = args map { _.asInstanceOf[AnyRef] }
    o.getClass.getMethods.find(x => x.getName == "apply" && x.isBridge).get.invoke(o, tmp: _*)
  }


  case class Qwerty(a: Int, b: String)

  def main(args: Array[String]) {
    val tokens = Parser.tokenize(myLispProgram)
    val tree = Parser.makeFullAST(tokens.toList).head

    val o1 = 1245
    val o2 = "dsafmnsaofmngaso"

    val res: Qwerty = createCaseClass(Qwerty, List(o1, o2)) //  map {_.asInstanceOf[AnyRef]})
    println(res)

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

