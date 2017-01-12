package core

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.Trees.Untyped
import dotty.tools.dotc.core.Types

object ADT {

  case class TypeTreeX[-T >: Untyped](value: TypeX) extends Trees.Tree {
    override def toString: String = {
      value match {
        case TypeNameX(ss) => ss
        case _ =>
          "(t " + value.toString + ")"
      }
    }
  }

  trait TypeX extends Types.Type {
    override def hash: Int = ???
  }

  case object NoPrefix extends TypeX

  case class TypeNameX(value: String) extends TypeX {
    override def toString: String = "(tn " + value + ")"
  }

  case class JavaArrayTypeX(value: TypeX) extends TypeX


}
