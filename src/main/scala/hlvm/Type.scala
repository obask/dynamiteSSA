package hlvm

import ast.{ASymbol, CodeTree, ABranch}

sealed trait Type
case object VoidTy extends Type
case object Int8Ty extends Type
case object Int32Ty extends Type
case object Int64Ty extends Type


case class PointerType(ch: Type) extends Type
case class StructType(name: String, fields: Option[Seq[Type]]) extends Type
case class FunctionType(ret: Type, args: Seq[Type]) extends Type


object Type {


  def LLVMPointerType(x: Type): Type = null


  def getPtrToOpaqueTypes(typeName: CodeTree): Type = {
    typeName match {
      case ABranch("Array", List(typeArg)) =>
        val x: Type = getTypeOfName(typeArg.toString)
        PointerType(x)
      case tp: ASymbol =>
        getTypeOfName(tp.value)
    }
  }

  def getTypeOfName(typeName: String): Type = {
    typeName match {
      case "EmptyTree" => VoidTy
      case "String" => PointerType(Int8Ty)
      case "Int" => Int32Ty
      case "Long" => Int64Ty
      case "Boolean" => Int8Ty
      case _ =>
        val pType = StructType(typeName, None)
        PointerType(pType)
    }
  }



}



