package goast

import core.ADT
import core.CodeUtils.createCaseClass
import sexpr._

object GoReader {

  def handleGoType(tree: CodeTree): ADT.TypeX = {
    println("handleType: " + tree)

    tree match {
      //      case ABranch("TypeRef", params) =>
      //        val tmp = TypeName(params(1).asInstanceOf[ASymbol].value)
      //        createCaseClass(TypeRef, Seq(handleType(params.head), tmp))

      case ABranch("t", params) =>
        handleGoType(tree)
      //        handleDefault(tree).asInstanceOf[Type]
//      case ABranch(cmd, params) =>
//        val args = params.map(handleGoType)
//        cmd match {
//        }
      case _ => handleDefault(tree).asInstanceOf[ADT.TypeX]
    }
  }

  def handleDefault(tree: CodeTree): Any = {
    println(tree)
    tree match {
      case _ => ???
    }
  }

  def handleGoAST(tree: CodeTree): Any = {
    //    println("----")
    //    println("handleAST: " + tree.toString.take(20))
    tree match {
      case ASymbol("true") =>
        true
      case ASymbol("false") =>
        false
      case ASymbol("nil") =>
        Nodes.NilNode
      case ASymbol(s) if Tokens.OTHERS.contains(s) =>
        Tokens.Wrapper(s)
      case ASymbol(x) =>
        // unquote string
        Nodes.Ident(x.substring(1, x.length - 1))
      case ABranch("%", params) =>
        params.map(handleGoAST)
      case ABranch("const", params) =>
        params match {
          case Seq(ANumber(x)) => Nodes.BasicLit(Tokens.INT, x.toString)
          case Seq(ADouble(x)) => Nodes.BasicLit(Tokens.FLOAT, x.toString)
          case ll: List[ASymbol] =>
            val tmp = ll.map(_.value).mkString(" ")
            Nodes.BasicLit(Tokens.STRING, tmp)
        }
      case ABranch(cmd, params) =>
        val args = params.map(handleGoAST)
        cmd match {
          case "BinaryExpr" => createCaseClass(Nodes.BinaryExpr, args)
          case "BadStmt" => createCaseClass(Nodes.BadStmt, args)
          case "ExprStmt" => createCaseClass(Nodes.ExprStmt, args)
          case "FuncLit" => createCaseClass(Nodes.FuncLit, args)
          case "ParenExpr" => createCaseClass(Nodes.ParenExpr, args)
          case "FuncType" => createCaseClass(Nodes.FuncType, args)
          case "BlockStmt" => createCaseClass(Nodes.BlockStmt, args)
          case "ArrayType" => createCaseClass(Nodes.ArrayType, args)
          case "AssignStmt" => createCaseClass(Nodes.AssignStmt, args)
          case "BranchStmt" => createCaseClass(Nodes.BranchStmt, args)
          case "Field" => createCaseClass(Nodes.Field, args)
          case "ForStmt" => createCaseClass(Nodes.ForStmt, args)
          case "CommClause" => createCaseClass(Nodes.CommClause, args)
          case "SendStmt" => createCaseClass(Nodes.SendStmt, args)
          case "SelectStmt" => createCaseClass(Nodes.SelectStmt, args)
          case "Ellipsis" => createCaseClass(Nodes.Ellipsis, args)
          case "IfStmt" => createCaseClass(Nodes.IfStmt, args)
          case "DeclStmt" => createCaseClass(Nodes.DeclStmt, args)
          case "DeferStmt" => createCaseClass(Nodes.DeferStmt, args)
          case "Package" => createCaseClass(Nodes.Package, args)
          case "CaseClause" => createCaseClass(Nodes.CaseClause, args)
          case "ImportSpec" => createCaseClass(Nodes.ImportSpec, args)
          case "BasicLit" => createCaseClass(Nodes.BasicLit, args)
          case "FuncDecl" => createCaseClass(Nodes.FuncDecl, args)
          case "SliceExpr" => createCaseClass(Nodes.SliceExpr, args)
          case "CommentGroup" => createCaseClass(Nodes.CommentGroup, args)
          case "InterfaceType" => createCaseClass(Nodes.InterfaceType, args)
          case "FieldList" => createCaseClass(Nodes.FieldList, args)
          case "ChanType" => createCaseClass(Nodes.ChanType, args)
          case "IncDecStmt" => createCaseClass(Nodes.IncDecStmt, args)
          case "CallExpr" => createCaseClass(Nodes.CallExpr, args)
          case "StructType" => createCaseClass(Nodes.StructType, args)
          case "BadDecl" => createCaseClass(Nodes.BadDecl, args)
          case "Ident" => createCaseClass(Nodes.Ident, args)
          case "Package" => createCaseClass(Nodes.Package, args)
          case "LabeledStmt" => createCaseClass(Nodes.LabeledStmt, args)
          case "EmptyStmt" => createCaseClass(Nodes.EmptyStmt, args)
          case "UnaryExpr" => createCaseClass(Nodes.UnaryExpr, args)
          case "GoStmt" => createCaseClass(Nodes.GoStmt, args)
          case "BadExpr" => createCaseClass(Nodes.BadExpr, args)
          case "RangeStmt" => createCaseClass(Nodes.RangeStmt, args)
          case "CompositeLit" => createCaseClass(Nodes.CompositeLit, args)
          case "MapType" => createCaseClass(Nodes.MapType, args)
          case "TypeSpec" => createCaseClass(Nodes.TypeSpec, args)
          case "StarExpr" => createCaseClass(Nodes.StarExpr, args)
          case "Comment" => createCaseClass(Nodes.Comment, args)
          case "SelectorExpr" => createCaseClass(Nodes.SelectorExpr, args)
          case "GenDecl" => createCaseClass(Nodes.GenDecl, args)
          case "SwitchStmt" => createCaseClass(Nodes.SwitchStmt, args)
          case "KeyValueExpr" => createCaseClass(Nodes.KeyValueExpr, args)
          case "ValueSpec" => createCaseClass(Nodes.ValueSpec, args)
          case "IndexExpr" => createCaseClass(Nodes.IndexExpr, args)
          case "TypeAssertExpr" => createCaseClass(Nodes.TypeAssertExpr, args)
          case "File" => createCaseClass(Nodes.File, args)
          case "TypeSwitchStmt" => createCaseClass(Nodes.TypeSwitchStmt, args)
          case "ReturnStmt" => createCaseClass(Nodes.ReturnStmt, args)

          case _ => handleDefault(tree)
        }
      case _ => handleDefault(tree)
    }
  }


}
