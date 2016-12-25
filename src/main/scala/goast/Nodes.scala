package goast

object Nodes {

  sealed abstract class Node extends Product
    with Cloneable {}

  case object NilNode extends Node {
    override def toString: String = "nil"
  }

  trait Expr extends Node

  trait Stmt extends Node

  trait Decl extends Node

  trait Spec extends Node

  type ChanDir = Int

  trait Scope

  case class BinaryExpr(
                         x: Expr,
                         op: Tokens.Token,
                         y: Expr
                       ) extends Expr

  case class BadStmt() extends Node

  case class ExprStmt(
                       x: Expr
                     ) extends Stmt

  case class FuncLit(
                      typex: FuncType,
                      body: BlockStmt
                    ) extends Expr

  case class ParenExpr(
                        x: Expr
                      ) extends Expr

  case class FuncType(
                       params: FieldList,
                       results: FieldList
                     ) extends Expr

  case class BlockStmt(
                        list: List[Stmt]
                      ) extends Stmt

  case class ArrayType(
                        len: Expr,
                        elt: Expr
                      ) extends Node

  case class AssignStmt(
                         lhs: List[Expr],
                         tok: Tokens.Token,
                         rhs: List[Expr]
                       ) extends Stmt

  case class BranchStmt(
                         tok: Tokens.Token,
                         label: Ident
                       ) extends Stmt

  case class Field(
                    names: List[Ident],
                    typex: Expr,
                    tag: Node
                  ) extends Node

  case class ForStmt(
                      init: Stmt,
                      cond: Expr,
                      post: Stmt,
                      body: BlockStmt
                    ) extends Stmt

  case class CommClause(
                         comm: Stmt,
                         body: List[Stmt]
                       ) extends Node

  case class SendStmt(
                       chan: Expr,
                       value: Expr
                     ) extends Stmt

  case class SelectStmt(
                         body: BlockStmt
                       ) extends Stmt

  case class Ellipsis(
                       elt: Expr
                     ) extends Node

  case class IfStmt(
                     init: Stmt,
                     cond: Expr,
                     body: BlockStmt,
                     otherwise: Stmt
                   ) extends Stmt

  case class DeclStmt(
                       decl: Decl
                     ) extends Stmt

  case class DeferStmt(
                        call: CallExpr
                      ) extends Stmt

  case class CaseClause(
                         list: List[Expr],
                         body: List[Stmt]
                       ) extends Node

  case class ImportSpec(
                         name: Node,
                         path: BasicLit
                       ) extends Spec

  case class BasicLit(
                       kind: Tokens.Token,
                       value: String
                     ) extends Expr

  case class FuncDecl(
                       recv: FieldList,
                       name: Ident,
                       typex: FuncType,
                       body: BlockStmt
                     ) extends Decl

  case class SliceExpr(
                        x: Expr,
                        low: Expr,
                        high: Expr,
                        max: Expr,
                        slice3: Boolean
                      ) extends Expr

  case class CommentGroup(
                           list: List[Comment]
                         ) extends Node

  case class InterfaceType(
                            methods: FieldList,
                            incomplete: Boolean
                          ) extends Expr

  case class FieldList(
                        list: List[Field]
                      ) extends Node

  case class ChanType(
                       dir: ChanDir,
                       value: Expr
                     ) extends Node

  case class IncDecStmt(
                         x: Expr,
                         tok: Tokens.Token
                       ) extends Stmt

  case class CallExpr(
                       fun: Expr,
                       args: List[Expr]
                     ) extends Expr

  case class StructType(
                         fields: FieldList,
                         incomplete: Boolean
                       ) extends Node

  case class BadDecl(
                    ) extends Decl

  case class Ident(
                    name: String
                  ) extends Expr

  case class Package(
                      name: String,
                      scope: Scope,
                      imports: Map[String, Object],
                      files: Map[String, File]
                    ) extends Node

  case class LabeledStmt(
                          label: Ident,
                          stmt: Stmt
                        ) extends Stmt

  case class EmptyStmt(
                        implicitX: Boolean
                      ) extends Stmt

  case class UnaryExpr(
                        op: Tokens.Token,
                        x: Expr
                      ) extends Expr

  case class GoStmt(
                     call: CallExpr
                   ) extends Stmt

  case class BadExpr(
                    ) extends Node

  case class RangeStmt(
                        key: Expr,
                        value: Expr,
                        tok: Tokens.Token,
                        x: Expr,
                        body: BlockStmt
                      ) extends Node

  case class CompositeLit(
                           typex: Expr,
                           elts: List[Expr]
                         ) extends Expr

  case class MapType(
                      key: Expr,
                      value: Expr
                    ) extends Node

  case class TypeSpec(
                       name: Ident,
                       typex: Expr
                     ) extends Spec

  case class StarExpr(
                       x: Expr
                     ) extends Expr

  case class Comment(
                      text: String
                    ) extends Node

  case class SelectorExpr(
                           x: Expr,
                           sel: Ident
                         ) extends Expr

  case class GenDecl(
                      tok: Tokens.Token,
                      specs: List[Spec]
                    ) extends Decl

  case class SwitchStmt(
                         init: Stmt,
                         tag: Expr,
                         body: BlockStmt
                       ) extends Stmt

  case class KeyValueExpr(
                           key: Expr,
                           value: Expr
                         ) extends Expr

  case class ValueSpec(
                        names: List[Ident],
                        typex: Expr,
                        values: List[Expr]
                      ) extends Spec

  case class IndexExpr(
                        x: Expr,
                        index: Expr
                      ) extends Expr

  case class TypeAssertExpr(
                             x: Expr,
                             typex: Expr
                           ) extends Expr

  case class File(
                   name: Ident,
                   decls: List[Decl],
                   scope: Node,
                   imports: List[ImportSpec],
                   unresolved: List[Ident],
                   comments: List[CommentGroup]
                 ) extends Node

  case class TypeSwitchStmt(
                             init: Stmt,
                             assign: Stmt,
                             body: BlockStmt
                           ) extends Stmt

  case class ReturnStmt(
                         results: List[Expr]
                       ) extends Stmt

}
