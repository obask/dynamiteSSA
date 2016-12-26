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
//      case Trees.Thicket(Nil) => "EmptyTree"

      //      case t: TypeRef =>
//        toSource(t.name)
      case ADT.TypeNameX(value) =>
        "(tn " + value + ")"
      case ADT.TypeTreeX(t) =>
        "(t " + toSource(t) + ")"

//      case t: TypeTree =>
//        t.original match {
//          case TypeRef(_, tn) => "(t " + tn.toString + ")"
//          case _ => "(TypeTree " + toSource(t.original) + ")"
//        }
      case p: Product => p.productIterator.map(toSource).mkString("(" + p.productPrefix + " ", " ", ")")
      case _ => p.toString
    }
  }

}
