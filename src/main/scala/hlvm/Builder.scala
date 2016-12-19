package hlvm

import parser.IRNode

import scala.collection.mutable


case class Builder() {



  var bb = BasicBlock.newBasicBlock("entry")

  def setInsertPoint(bb1: BasicBlock) = {
    bb = bb1
  }

  def createRet(v: Value) = {
    val tmp = Return(bb.currentPos, v)
    bb.append(tmp)
    tmp
  }

  def createAlloca(v: Type, twine: String) = {
    val name =  IRNode.CurrentFunction.getNewName(twine)
    val tmp = Alloca(bb.currentPos, v, name)
    bb.append(tmp)
    tmp
  }

  def createAlloca(v: Type, size: Value, twine: String): Value = {
    val tmp = AllocaArray(null, v, size, twine)
    bb.append(tmp)
    tmp
  }

  def createLoad(ptr: Value): Value = null

  def createStore(value: Value, ptr: Value): Value = {
    val tmp = Store(null, value, ptr)
    bb.append(tmp)
    tmp
  }

  def createGEP(ptr: Value, idxList: Array[Value]) = null


  def createCondBr(cond: Value, thenBB: BasicBlock, elseBB: BasicBlock) = {
    val tmp = Cond(bb.currentPos, Int32Ty, cond, thenBB, elseBB)
    bb.append(tmp)
    tmp
  }


  def createCall(fn: Function, args: Seq[Value]): Value = {
    val tmp = CallInst(bb.currentPos, fn, args)
    bb.append(tmp)
    tmp
  }



}
