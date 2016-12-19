package hlvm

import parser.IRNode

import scala.collection.mutable

case class Function(name: String, t: FunctionType, args: Seq[String], bb: BasicBlock) {

  val names = mutable.Map[String, Int]()

  def getNewName(name: String) = {
    val res = names.getOrElse(name, 0) + 1
    names(name) = res
    "$" + name + res
  }

  val getType: Type = this.t

  def getRetType: Type = t.ret

  def codegen: Seq[String]  = {
    print(t)
    Seq(this.t.ret.repr) ++
    Seq(this.name + " (" + this.t.args.map(_.repr).mkString(", ")) ++
    Seq("{") ++
    Pretty.shiftRight(bb.code.flatMap(_.codegen)) ++
    Seq("}")
  }

  def repr: String = {
    this.name
  }

}
