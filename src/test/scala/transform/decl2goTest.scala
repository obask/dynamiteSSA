package transform

import dotty.tools.dotc.ast.untpd
import goast.{GoPrinter, Nodes}
import org.scalatest.{FlatSpec, Matchers}
import sexpr.Parser

import scalast.DReader

class decl2goTest extends FlatSpec with Matchers {

  implicit def convertExpr(ss: String): Nodes.Node = {
    val tree = Parser.makeFullAST(Parser.tokenize(ss)).head
    val ast = DReader.handleDottyAST(tree).asInstanceOf[untpd.Tree]
    def2Golang.helper(ast)
  }

  GoPrinter.toSource(
    "(DefDef fact Nil [% [% (ValDef tmp (t Int) EmptyTree)]] (t Int) (Literal (Constant 1)))"
  ) should be ("""
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
