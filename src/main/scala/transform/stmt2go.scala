package transform

import dotty.tools.dotc.ast.{Trees, untpd}
import goast.Nodes._
import goast.{Nodes, Tokens}

object stmt2go {

  def apply(ast: untpd.Tree): Nodes.Stmt = {
    ast match {

      case Trees.Block(stats, expr) =>
        Nodes.BlockStmt(
          stats.map(stmt2go(_)) :+
            Nodes.ReturnStmt(List(expr2go(expr)))
        )

      case Trees.ValDef(name, _, body) =>
        AssignStmt(List(Ident(name.toString)), Tokens.Wrapper(":="),
          List(expr2go(body.asInstanceOf[untpd.Tree])))

      case Trees.If(cond, br1, br2) =>
        IfStmt(null, expr2go(cond),
          BlockStmt(List(ReturnStmt(List(expr2go(br1))))),
          BlockStmt(List(ReturnStmt(List(expr2go(br2)))))
        )

    }
  }

}
