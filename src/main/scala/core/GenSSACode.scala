package core

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Names.{TermName, TypeName}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.dotc.printing.RefinedPrinter

import scala.collection.mutable

class GenSSACode extends Phase {
  def phaseName: String = "genSSACode"
  private val entryPoints = new mutable.HashSet[Symbol]()
  def registerEntryPoint(sym: Symbol) = entryPoints += sym

  def run(implicit ctx: Context): Unit = {
//    new GenBCodePipeline(entryPoints.toList, new DottyBackendInterface()(ctx))(ctx).run(ctx.compilationUnit.tpdTree)


    val tree = ctx.compilationUnit.tpdTree
//    val tree = ctx.compilationUnit.untpdTree


    //    val printer = new Printer()



    println("----------------")

    val printer = new RefinedPrinter(ctx)
    println(printer.toText(tree).mkString(width = 100))

    println("----------------")

    def toSource(p: Any): String = {
      p match {
        case s: String => "\"" + s + "\""
        case () => "Unit"
        case n: TermName if n.isEmpty => "(TermName )"
        case n: TypeName => "(tn " + n.toString + ")"

        case List() => "Nil"
        case ll: List[_] => ll.map(toSource).mkString("[% ", " ", "]")
        case tpd.EmptyTree => "EmptyTree"
        // TODO recover full path to types
        case t: TypeRef =>
          toSource(t.name)
        case t: Trees.TypeTree[_] =>
//          val tmp = if (t.hasType) toSource(t.typeOpt) else toSource(t.tpe)
//          "(TypeTree " + tmp + ")"
//          TODO set different tags for hasType and reference

          if (t.hasType) {
            t.typeOpt match {
              case TypeRef(_, tn) => "(t " + tn.toString + ")"
              case _ => "(TypeTree " + toSource(t.typeOpt) + ")"
            }
          }
          else
            "(TypeTreeX " + toSource(t.tpe) + ")"
        case p: Product => p.productIterator.map(toSource).mkString("(" + p.productPrefix + " ", " ", ")")
        case _ => p.toString
      }
    }

    println(toSource(tree))

    println("----------------")

    //    println("AST:")
//    tree match {
//      case xx: PackageDef =>
//        printer.printListField("AST", xx.stats)
//    }

//    println(tree)
//    println("----------------")

    //    Lispyfy.processPackageDef(tree.asInstanceOf[PackageDef])


    entryPoints.clear()
  }




}
