package hlvm


case class Position(curr: Int) {
  def next = Position(curr+1)
}


sealed trait Value {

  val getType: Type

}

case class ConstantLong(p: Position, value: Long) extends Value {

  val getType: Type = Int32Ty

}

case class ConstantString(p: Position, value: String) extends Value {

  val getType: Type = PointerType(Int8Ty)

}

case class PHINode(p: Position) extends Value {

  def addIncoming(v: Value, bb: BasicBlock) = null

  val getType: Type = null

}


case class Alloca(p: Position, ty: Type) extends Value {

  val getType: Type = ty

}

case class AllocaArray(p: Position, ty: Type, cnt: Value) extends Value {

  val getType: Type = ty

}

case class CallInst(p: Position, fn: hlvm.Function, args: Seq[Value]) extends Value {

  val getType: Type = fn.getRetType

}
