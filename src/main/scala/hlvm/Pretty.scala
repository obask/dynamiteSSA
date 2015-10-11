package hlvm

object Pretty {

  def shiftRight(data: Seq[String]): Seq[String] = {
    for (x <- data) yield "    " + x
  }

}
