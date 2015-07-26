package hlvm

case class Function(name: String, t: FunctionType, bb: BasicBlock) {

  def getRetType: Type = t.ret

}
