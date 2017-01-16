package transform

import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.core.Constants
import goast.Nodes._
import goast.{Nodes, Tokens}

object expr2go {

  def apply(ast: untpd.Tree): Nodes.Expr = {
    ast match {
      case Trees.Ident(name) =>
        Ident(name.toString)
      case Trees.Literal(c) =>
        c.tag match {
          case Constants.IntTag => Nodes.BasicLit(Tokens.INT, c.value.toString)
        }
      case Trees.Apply(Trees.Select(Trees.Ident(name), methodName), args) =>
        methodName.toString match {
          case "$minus" =>
            BinaryExpr(Ident(name.toString), Tokens.Wrapper("-"), expr2go(args.head))
        }
      //      (FuncType (FieldList [%]) (FieldList [% (Field [%] "int" nil)]))
      //      (CallExpr (FuncLit ... (BlockStmt [% (ReturnStmt [% "tmp"])])) [%])
      case Trees.Typed(expr, tpt) =>
        // "typed expr" means: wrap field into lambda function without args
        val returnField = Field(List(), expr2go(tpt), Nodes.NilNode)
        val funcType = FuncType(FieldList(Nil), FieldList(List(returnField)))
        val blockStmt = BlockStmt(List(ReturnStmt(List(expr2go(expr)))))
        CallExpr(FuncLit(funcType, blockStmt), List())

    }
  }


}
