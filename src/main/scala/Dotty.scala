import dotty.tools.dotc.ast.Trees.Untyped
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.printing.RefinedPrinter
import goast.Nodes

object Dotty extends App {

  def createCaseClass[T, T1](o: T1, args: Seq[Any]): T = {
    println("createCaseClass: " + o.getClass.getName)
    println("args " + args.length.toString + ": " + args.map(_.toString.take(100)).mkString(" |#| "))
    val tmp = args map { _.asInstanceOf[AnyRef] }
    println(o.getClass.getMethods.toSeq.mkString("\n"))
    o.getClass.getMethods
      .find(x => x.getName == "apply" && !x.isBridge)
      .get.invoke(o, tmp: _*)
      .asInstanceOf[T]
  }


  val path = "/Users/baskakov/IdeaProjects/dynamiteSSA/src/main/resources/result.sexpr"

  implicit def ctx: Context = (new ContextBase).initialCtx


  val tree = Trees.Apply(Trees.Ident(Names.termName("ololo")), List())


  val tmp = List(Trees.Ident(Names.termName("ololo")), List()).toArray


//  val qqq = tpd.Apply(tpd.Ident(Names.termName("ololo")), List())

  val res: Trees.Tree[Untyped] = createCaseClass(Trees.Apply, tmp)

  //  println(Trees.Apply.getClass.getMethods.toList.map(_.getName))
    println(res)

  val printer = new RefinedPrinter(ctx)
  println(printer.toText(res).mkString(width = 100))

}
