package transform

import dotty.tools.dotc.ast.untpd
import goast.Nodes.Expr
import goast.{GoPrinter, Nodes}
import org.scalatest.{FlatSpec, Matchers}
import sexpr.Parser

import scalast.DReader

class def2GolangTest extends FlatSpec with Matchers {

  implicit def convertExpr(ss: String): Nodes.Node = {
    val tree = Parser.makeFullAST(Parser.tokenize(ss)).head
    val ast = DReader.handleDottyAST(tree).asInstanceOf[untpd.Tree]
    def2Golang.helper(ast)
  }

  def stmt(ss: String): Nodes.Stmt = {
    val tree = Parser.makeFullAST(Parser.tokenize(ss)).head
    val ast = DReader.handleDottyAST(tree).asInstanceOf[untpd.Tree]
    def2Golang.helperStmt(ast)
  }

  GoPrinter.toSource(
    "(Literal (Constant 1))") should
    be ("(const 1)")

  GoPrinter.toSource(
    "(Apply (Select (Ident n) $minus) [% (Ident i)])") should
    be ("(BinaryExpr \"n\" \"-\" \"i\")")

  GoPrinter.toSource(
    "(Typed (Ident tmp) (t Int))") should
    be ("(CallExpr (FuncLit (FuncType (FieldList [% ]) (FieldList [% (Field [% ] \"int\" nil)])) (BlockStmt [% (ReturnStmt [% \"tmp\"])])) [% ])")


  GoPrinter.toSource("(DefDef fact Nil [% [% (ValDef tmp (t Int) EmptyTree)]] (t Int) (Literal (Constant 1)))") should
    be ("""
(FuncDecl
        nil
        "fact"
        (FuncType
          (FieldList
            [% (Field
                  [% "tmp"
                      ]
                  "int"
                  nil
                  )
                ]
            )
          (FieldList
            [% (Field
                  [% ]
                  "int"
                  nil
                  )
                ]
            )
          )
        (BlockStmt
          [% (ReturnStmt
                [% (const 1)
                    ]
                )
              ]
          )
        )"""
      .replace("  ", "")
      .replace("\n", " ")
      .replace("  ", " ")
  )

}
