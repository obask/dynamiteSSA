package transform

import core.ADT.{TypeNameX, TypeTreeX}
import dotty.tools.dotc.ast.{Trees, untpd}
import goast.Nodes
import goast.Nodes._

object type2go {

  def apply(tree: untpd.Tree): Nodes.Expr = {
    tree match {
    case TypeTreeX(TypeNameX("Int")) =>
      Ident("int")
    }
  }


}
