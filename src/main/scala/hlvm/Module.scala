package hlvm

import scala.collection.mutable

case class Module() {


  val functions = mutable.Map[String, Function]()

  functions("newString") = Function("newString", FunctionType(Int32Ty, Seq(PointerType(Int8Ty))), null, null)

  def getTypeByName(s: String): Type = functions(s).t

  def functionCreate(ft: FunctionType, externalLinkage: Boolean, name: String): Function = {
    println("ft = " + ft)
    val fun = Function(name, ft, null, null)
    functions(name) = fun
    fun
  }


  def functionSetBody(name: String, args: Seq[String], bb: BasicBlock): Function = {
    val fun = functions(name)
    functions(name) = Function(fun.name, fun.t, args, bb)
    fun
  }


  def getFunction(name: String): Function = {
    functions(name)
  }



}
