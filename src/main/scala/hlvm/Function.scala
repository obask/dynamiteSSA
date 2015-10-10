package hlvm

import ast.IRNode

import scala.collection.mutable

case class Function(name: String, t: FunctionType, bb: BasicBlock) {

  val names = mutable.Map[String, Int]()

  def getNewName(name: String) = {
    val res = names.getOrElse(name, 0) + 1
    names(name) = res
    "$" + name + res
  }



  def getRetType: Type = t.ret

  def dump() = {
    println("TYPE:", this.t)
    Pretty.printLine("FUNCTION: " + name + " {")
    Pretty.shiftRight()
    for (xx <- bb.code) {
      xx.dump()
    }
    Pretty.shiftLeft()
    Pretty.printLine("}")
  }


}
