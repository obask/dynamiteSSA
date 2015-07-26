package hlvm

import scala.collection.mutable

case class Module() {


  val functions = mutable.Map[String, Function]()

  functions("newString") = Function("newString", FunctionType(Int32Ty, Seq(PointerType(Int8Ty))), null)


  def getTypeByName(s: String): Type = null


  def functionCreate(ft: FunctionType, externalLinkage: Boolean, name: String): Function = {
    val fun = Function(name, ft, null)
    functions(name) = fun
    fun
  }

  def getFunction(name: String) = {
    functions(name)
  }


}
