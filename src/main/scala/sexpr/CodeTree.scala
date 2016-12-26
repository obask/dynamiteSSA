package sexpr

sealed abstract class CodeTree

case class ABranch(cmd: String, params: List[CodeTree]) extends CodeTree {
  override def toString = {
    "(" + cmd + " " + (params map {_.toString} mkString " ") + ")"
  }
}

object ALeaf {
  def unapply(ct: CodeTree): Option[ALeaf] =
    ct match {
      case leaf: ALeaf => Some(leaf)
      case _ => None
    }
}

sealed abstract class ALeaf extends CodeTree
case class ASymbol(value: String) extends ALeaf {
  override def toString = value.toString
}

case class ANumber(value: Int) extends ALeaf {
  override def toString = value.toString
}

case class ADouble(value: Double) extends ALeaf {
  override def toString = value.toString
}

