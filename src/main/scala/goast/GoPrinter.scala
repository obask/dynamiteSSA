package goast

import goast.Nodes.{BasicLit, Ident, NilNode}

object GoPrinter {

    def toSource(p: Any): String = {
      println(p.getClass.toString)
      p match {
        case s: String => ???
        //          "\"" + s + "\""
        case Tokens.Wrapper(w) => w
        case NilNode => "nil"
        case Ident(n) => '"' + n.toString + '"'
        case c: BasicLit => "(const " + c.value + ")"
        case ll: List[_] => ll.map(toSource).mkString("[% ", " ", "]")
        case p: Product => p.productIterator
          .map(toSource)
          .mkString("(" + p.productPrefix + " ", " ", ")")
        case _ => p.toString
      }
    }

}
