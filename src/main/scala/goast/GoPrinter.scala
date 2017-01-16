package goast

import goast.Nodes.{BasicLit, Ident, NilNode}

object GoPrinter {

  def toSourceExpr(p: Nodes.Expr): String = {
    toSource(p)
  }

    def toSource(p: Nodes.Node): String = {
      println(p.getClass.toString)
      p match {
        case Nodes.NilNode => "nil"
        case Nodes.Ident(n) => '"' + n.toString + '"'
        case c: Nodes.BasicLit => "(const " + c.value + ")"
        case _ => toSourceX(p)
      }
    }

  private def toSourceX(p: Any): String = {
    if (p != null) {
      println(p.getClass.toString)
    }
    p match {
      case null => "nil"
      case s: String => ???
      case Tokens.Wrapper(w) => "\"" + w + "\""
      case ll: List[_] => ll
        .map {case x: Nodes.Node => toSource(x)
              case x => toSourceX(x)
        }.mkString("[% ", " ", "]")
      case p: Product => p.productIterator
        .map {case x: Nodes.Node => toSource(x)
              case x => toSourceX(x)
        }
        .mkString("(" + p.productPrefix + " ", " ", ")")
      case _ => p.toString
    }
  }

}
