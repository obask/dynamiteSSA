package hlvm

import ast.CodeTree

import scala.collection.mutable


case class Builder() {

  var bb: BasicBlock = null

  def setInsertPoint(bb1: BasicBlock) = {
    bb = bb1
  }


  var currentPos = Position(0)


  def createRet(v: Value) = null

  def createAlloca(v: Type) = {
    currentPos = currentPos.next
    Alloca(currentPos, v)
  }

  def createAlloca(v: Type, size: Value, twine: String): Value = {
    currentPos = currentPos.next
    AllocaArray(currentPos, v, size)
  }

  def createLoad(ptr: Value): Value = null

  def createStore(value: Value, ptr: Value): Value = null

  def createGEP(ptr: Value, idxList: Array[Value]) = null


  def createCondBr(cond: Value, thenBB: BasicBlock, elseBB: BasicBlock) = null


  def createPHI(t: Type, n: Int, twine: String): PHINode = null

  def createCall(fn: Function, args: Seq[Value]): Value = {
    currentPos = currentPos.next
    CallInst(currentPos, fn, args)
  }



}
