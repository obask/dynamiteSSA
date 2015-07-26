package hlvm


case class Position(curr: Int) {
  def next = Position(curr+1)

  override def toString = {
    "$" + curr
  }

}


sealed trait Value {


  def dump(): Unit

  val getType: Type

  val pos: Position


  def repr: String

}

case class ConstantLong(pos: Position, value: Long) extends Value {

  val getType: Type = Int32Ty

  def dump(): Unit = {
    println(value)
  }


  def repr: String = value.toString


}

case class ConstantString(pos: Position, value: String) extends Value {

  val getType: Type = PointerType(Int8Ty)

  def dump(): Unit = {
//    println(getType + pos.toString + " = " + value + ";")
    println(value)

  }

  def repr: String = "\"" + value.toString + "\""

}

case class PHINode(pos: Position) extends Value {

  def addIncoming(v: Value, bb: BasicBlock) = null

  val getType: Type = null

  def dump(): Unit = ???

  def repr: String = ???

}


case class Alloca(pos: Position, ty: Type) extends Value {

  val getType: Type = ty

  def dump(): Unit = {
    println(getType.repr + " " + pos.toString + ";")
  }


  def repr: String = pos.toString


}


case class AllocaArray(pos: Position, ty: Type, cnt: Value) extends Value {

  val getType: Type = ty

  def dump(): Unit = {
    println(getType.repr + " " + pos.toString + "[" + cnt.repr + "];")
  }

  def repr: String = pos.toString

}

case class CallInst(pos: Position, fn: hlvm.Function, args: Seq[Value]) extends Value {

  val getType: Type = fn.getRetType


  def dump(): Unit = {
    println(getType.repr + " " + pos.toString + " = " + fn.name + args.map(_.repr) + ";")
  }

  def repr: String = pos.toString

}
