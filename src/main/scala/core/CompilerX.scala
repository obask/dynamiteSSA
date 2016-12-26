package core

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.ContextBase
import dotty.tools.dotc.core.Phases.Phase

class CompilerX extends Compiler {

  protected def initCtx = (new ContextBase).initialCtx

  override def phases: List[List[Phase]] = {
    val oldPhases = super.phases
    oldPhases.updated(oldPhases.length - 1, List(new GenSSACode))
  }

}
