package hlvm

object Pretty {

  var shift = 0

  def shiftRight() = {
    shift += 1
  }

  def shiftLeft() = {
    assert(shift > 0)
    shift -= 1
  }

  def printLine(ss: Any): Unit = {
    println("    " * shift + ss)
  }



}
