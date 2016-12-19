package hlvm

import parser.{ABranch, ASymbol, CodeTree}

sealed trait Type {
  def repr: String

}

trait SimpleType extends Type {
  def repr: String = {
    toString
  }
}

case object VoidTy extends SimpleType
case object Int8Ty extends SimpleType
case object Int32Ty extends SimpleType
case object Int64Ty extends SimpleType

case class PointerType(ch: Type) extends Type {
    def repr: String = {
      ch.repr + "*"
    }
}

case class StructType(name: String, fields: Option[Seq[Type]]) extends Type {
  def repr: String = {
    name
  }
}

case class FunctionType(ret: Type, args: Seq[Type]) extends Type {
  def repr: String = {
    ret + "(* lambda)(" + args.map(_.repr).mkString(", ") + ")"
  }
}


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



