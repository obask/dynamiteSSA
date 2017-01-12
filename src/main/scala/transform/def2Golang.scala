package transform

import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.{Constants, Names}
import goast.{Nodes, Tokens}
import goast.Nodes.{AssignStmt, BinaryExpr, Ident}

object def2Golang {

  implicit def ctx: Context = (new ContextBase).initialCtx

  case class WrapX(e: Nodes.Expr)

  def apply(input: untpd.DefDef): untpd.DefDef = {
    input.copy(preRhs =
      helper(input.unforced.asInstanceOf[untpd.Tree])
    )
  }

  def helperReturn(ast: untpd.Tree): Nodes.Stmt = {
    null
  }

  def helperStmt(ast: untpd.Tree): Nodes.Stmt = {
    ast match {
      // (ValDef n1 (t Int) (Apply (Select (Ident n) $minus) [% (Literal (Constant 1))]))
      case Trees.ValDef(name, tpt, body) =>
        // (AssignStmt [% "n1"] ":=" [% (BinaryExpr "n" "-" (const 1))])
        AssignStmt(List(Ident("n1")), Tokens.Wrapper(":="),
          List(helperExpr(body.asInstanceOf[untpd.Tree])))
    }

  }

//  def helperBinExpr(name: Names.Name, method:Names.Name, args: ): Nodes.BinaryExpr = {
//    ast match {
//      case Trees.Apply(Trees.Select(Trees.Ident(name), methodName), args) =>
//        methodName match {
//          case
//
//        }
//    }
//
//  }


    def helperExpr(ast: untpd.Tree): Nodes.Expr = {
    ast match {
      case Trees.Literal(c) =>
        c.tag match {
          case Constants.IntTag => Nodes.BasicLit(Tokens.INT, c.toString)
        }
      case Trees.Apply(Trees.Select(Trees.Ident(name), methodName), args) =>
        methodName.toString match {
          case "$minus" =>
            BinaryExpr(Ident(name.toString), Tokens.Wrapper("-"), helperExpr(args.head))
        }

    }
  }

  def helper(ast: untpd.Tree): Nodes.Node = {
//    println("helper: " + ast)
    ast match {
      case Trees.Block(stats, expr) =>
        Nodes.BlockStmt(
          stats.map(helperStmt) :+
            Nodes.ReturnStmt(List(helperExpr(expr)))
        )
  }

    null
  }

}
