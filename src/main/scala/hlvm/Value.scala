package hlvm


case class Position(curr: Int) {
  def next = Position(curr+1)

  override def toString = {
    "$" + curr
  }

}


sealed trait Value {


  def codegen: Seq[String]

  val getType: Type

  val pos: Position


  def repr: String

}




case class ConstantLong(pos: Position, value: Long) extends Value {

  val getType: Type = Int32Ty

  def codegen: Seq[String]  = {
    Seq(value.toString)
  }

  def repr: String = value.toString

}

case class ConstantString(pos: Position, value: String) extends Value {

  val getType: Type = PointerType(Int8Ty)

  def codegen: Seq[String]  = {
//    println(getType + pos.toString + " = " + value + ";")
    Seq(value)
  }

  def repr: String = "\"" + value.toString + "\""

}

case class Store(pos: Position, src: Value, dst: Value) extends Value {

  val getType: Type = dst.getType

  def codegen: Seq[String]  = {
    Seq(dst.repr + " = " + src.repr + ";")
  }

  def repr: String = null

}


case class Return(pos: Position, v: Value) extends Value {

  val getType: Type = v.getType

  def codegen: Seq[String]  = {
    Seq("return " + v.repr + ";")
  }

  def repr: String = null

}



case class Cond(pos: Position, ty: Type, cond: Value, thenBr: BasicBlock, elseBr: BasicBlock) extends Value {

  val getType: Type = ty

  def codegen: Seq[String]  = {
    Seq("if (" + cond.repr + ") {") ++
      Pretty.shiftRight(thenBr.code.flatMap(_.codegen)) ++
      Seq("} else {") ++
      Pretty.shiftRight(elseBr.code.flatMap(_.codegen)) ++
      Seq("}")
  }

  def repr: String = pos.toString

}

case class Alloca(pos: Position, ty: Type, twine: String) extends Value {

  val getType: Type = ty

  def codegen: Seq[String]  = {
    Seq(getType.repr + " " + twine + ";")
  }

  def repr: String = twine

}

case class AllocaArray(pos: Position, ty: Type, cnt: Value, twine: String) extends Value {

  val getType: Type = ty

  def codegen: Seq[String]  = {
    Seq(getType.repr + " " + twine + "[" + cnt.repr + "];")
  }

  def repr: String = pos.toString

}

case class CallInst(pos: Position, fn: hlvm.Function, args: Seq[Value]) extends Value {

  val getType: Type = fn.getRetType


  def codegen: Seq[String]  = {
    Seq("const " + getType.repr + " " + pos.toString + " = " + fn.name + args.map(_.repr) + ";")
  }

  def repr: String = pos.toString

}

