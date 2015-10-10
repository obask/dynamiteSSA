package hlvm

import scala.collection.mutable



case class BasicBlock(s: String, code: mutable.ArrayBuffer[Value]) {

  var currentPos = Position(0)


  def append(tmp: Value) = {
    code.append(tmp)
    currentPos = currentPos.next
  }


  def dump(): Unit = {
    var ss = ""
    println("/* Basic Block: " + s + " */")
    for (v <-  code) {
      println("  " + v.dump() + "")
    }
  }

}



object BasicBlock {

  def newBasicBlock(s: String) = {
    BasicBlock(s, mutable.ArrayBuffer[Value]())
  }

}