package transform

import dotty.tools.dotc.ast.untpd
import goast.{GoPrinter, Nodes}
import org.scalatest.{FlatSpec, Matchers}
import sexpr.Parser

import scalast.DReader

class stmt2GolangTest extends FlatSpec with Matchers {

  def stmt(ss: String): Nodes.Stmt = {
    val tree = Parser.makeFullAST(Parser.tokenize(ss)).head
    val ast = DReader.handleDottyAST(tree).asInstanceOf[untpd.Tree]
    stmt2go(ast)
  }

  GoPrinter.toSource(stmt(
    "(ValDef tmp (t Int) (Literal (Constant 100)))")) should
    be ("(AssignStmt [% \"tmp\"] \":=\" [% (const 100)])")

  GoPrinter.toSource(stmt(
    "(ValDef tmp (t Int) (Ident n1))")) should
    be ("(AssignStmt [% \"tmp\"] \":=\" [% \"n1\"])")

  GoPrinter.toSource(stmt(
    "(If (Ident tmp) (Literal (Constant 1)) (Literal (Constant 2)))")) should
    be ("(IfStmt nil \"tmp\" (BlockStmt [% (ReturnStmt [% (const 1)])]) (BlockStmt [% (ReturnStmt [% (const 2)])]))")

}
