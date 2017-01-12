package transform

import core.ADT
import core.ADT.{TypeNameX, TypeTreeX}
import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names

object method2Function {

  implicit def ctx: Context = (new ContextBase).initialCtx

  // if file is an object
  def apply(input: untpd.Tree): untpd.Tree = {
    val tree = input.asInstanceOf[untpd.PackageDef]
    val moduleName = getModuleObjectName(tree)
    val moduleType = getModuleObjectType(tree)
    // extract code of all functions
    var result = List[untpd.DefDef]()
    val treeCopy = tree.copy(stats = tree.stats map {
      case td@Trees.TypeDef(name, tpl: untpd.Template)
        if name.toString != moduleType
      =>
        println("tpl.constr = " + tpl.constr)
        result +:= tpl.constr.copy(name = Names.termName("New" + name.toString))
        td.copy(rhs =
          tpl.copy(
            constr = tpl.constr.copy(preRhs = Trees.theEmptyTree),
            preBody = tpl.body map {
              case dd: untpd.DefDef =>
                result = addThatIntoFunctionParameters(dd, name) :: result
                val termName = Names.termName(dd.name.toString + "XXX")
                dd.copy(preRhs = Trees.theEmptyTree)
              case other => other
            }))
      case other => other
    })
    // save all function bodies into package object
    appendFunctionDefinitions(treeCopy, result)
  }

  private def getModuleObjectType(tree: untpd.PackageDef): String = {
    tree.stats.last.asInstanceOf[untpd.ValDef].tpt match {
      case TypeTreeX(TypeNameX(name)) => name
    }
  }

  private def getModuleObjectName(tree: untpd.PackageDef): String = {
    tree.stats.last.asInstanceOf[untpd.ValDef].name.toString
  }

  private def addThatIntoFunctionParameters(fun: untpd.DefDef, className: Names.TypeName)
  : untpd.DefDef = {
    fun.copy(vparamss = fun.vparamss.map(ll => {
      val typeTreeX = ADT.TypeTreeX(ADT.TypeNameX(className.toString))
      Trees.ValDef(core.THIS_PARAMETER_NAME, typeTreeX, Trees.theEmptyTree) :: ll
    }))
  }



  private def appendFunctionDefinitions(tree: untpd.PackageDef, funs: Seq[untpd.DefDef])
  : untpd.PackageDef = {
    // TODO check if function exists
    val moduleName = getModuleObjectType(tree)
    tree.copy(stats = tree.stats map {
      case td@Trees.TypeDef(name, tpl: untpd.Template)
        if name.toString == moduleName =>
        tpl.body.foreach(println)
        td.copy(rhs =
          tpl.copy(preBody =
            tpl.body ++ funs
          ))
      case other => other
    })
  }




}
