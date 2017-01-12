package scalast

import core.ADT
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Names

object DPrinter {

  def toSource(p: Any): String = {
    p match {
      case s: String => "\"" + s + "\""
      case () => "Unit"
      case n: Names.TermName if n.isEmpty => "(TermName )"
      case List() => "Nil"
      case ll: List[_] => ll.map(toSource).mkString("[% ", " ", "]")
      case Trees.theEmptyTree => "EmptyTree"
      case x: Trees.EmptyValDef[_] => "EmptyValDef"
//      case Trees.Thicket(Nil) => "EmptyTree"
      case ADT.TypeNameX(value) =>
        "(tn " + value + ")"
      case ADT.TypeTreeX(t) =>
        "(t " + toSource(t) + ")"
      case p: Product => p.productIterator.map(toSource).mkString("(" + p.productPrefix + " ", " ", ")")
      case _ => p.toString
    }
  }

}
