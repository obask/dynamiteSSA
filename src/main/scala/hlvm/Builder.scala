package hlvm

import ast.CodeTree

import scala.collection.mutable


case class Builder() {

  var bb = BasicBlock("entry")

  def setInsertPoint(bb1: BasicBlock) = {
    bb = bb1
  }

  def createRet(v: Value) = null

  def createAlloca(v: Type) = {
    val tmp = Alloca(bb.currentPos, v)
    bb.append(tmp)
    tmp
  }

  def createAlloca(v: Type, size: Value, twine: String): Value = {
    val tmp = AllocaArray(bb.currentPos, v, size)
    bb.append(tmp)
    tmp
  }

  def createLoad(ptr: Value): Value = null

  def createStore(value: Value, ptr: Value): Value = null

  def createGEP(ptr: Value, idxList: Array[Value]) = null


  def createCondBr(cond: Value, thenBB: BasicBlock, elseBB: BasicBlock) = null


  def createPHI(t: Type, n: Int, twine: String): PHINode = null

  def createCall(fn: Function, args: Seq[Value]): Value = {
    val tmp = CallInst(bb.currentPos, fn, args)
    bb.append(tmp)
    tmp
  }



}
