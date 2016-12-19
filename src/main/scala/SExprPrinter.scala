import ast.Trees.TypeTree
import ast.{TermName, Trees, TypeName}
import core.Types.TypeRef

object SExprPrinter {

  def toSource(p: Any): String = {
    p match {
      case s: String => "\"" + s + "\""
      case () => "Unit"
      case n: TermName if n.isEmpty => "(TermName )"
      case n: TypeName => "(tn " + n.toString + ")"

      case List() => "Nil"
      case ll: List[_] => ll.map(toSource).mkString("[% ", " ", "]")
      case Trees.theEmptyTree => "EmptyTree"
      // TODO recover full path to types
      case t: TypeRef =>
        toSource(t.name)
      case t: TypeTree =>
        //          val tmp = if (t.hasType) toSource(t.typeOpt) else toSource(t.tpe)
        //          "(TypeTree " + tmp + ")"
        //          TODO set different tags for hasType and reference
        //          tmp
        t.original match {
          case TypeRef(_, tn) => "(t " + tn.toString + ")"
          case _ => "(TypeTree " + toSource(t.original) + ")"
        }
      case p: Product => p.productIterator.map(toSource).mkString("(" + p.productPrefix + " ", " ", ")")
      case _ => p.toString
    }
  }

}
