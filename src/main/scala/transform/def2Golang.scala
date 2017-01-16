package transform

import core.ADT.{TypeNameX, TypeTreeX}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import goast.Nodes
import goast.Nodes._

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


  def helpTpt(tpt: untpd.Tree): Nodes.Expr = {
  }

  def helper(ast: untpd.Tree): Nodes.Node = {
//    println("helper: " + ast)
    ast match {
      case o => expr2go(o)
    }
  }

}
