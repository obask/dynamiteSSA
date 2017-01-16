package transform

import dotty.tools.dotc.ast.{Trees, untpd}
import goast.Nodes
import goast.Nodes._

object decl2go {

  def apply(tree: untpd.Tree): Nodes.Node = {
    tree match {
      case dd@Trees.DefDef(name, Nil, List(vparams), tpt, _) =>
        val params = vparams.map(x => {
          // (Field [% "tmp"] "int" nil)
          Field(List(Ident(x.name.toString)), type2go(tpt), Nodes.NilNode)
        })
        val returnField = Field(List(), type2go(tpt), Nodes.NilNode)
        val funcType = FuncType(FieldList(Nil), FieldList(List(returnField)))
        val blockStmt = BlockStmt(List(ReturnStmt(List(expr2go(dd.forceIfLazy)))))
        FuncDecl(null, Ident(name.toString), funcType, blockStmt)

    }
  }

}