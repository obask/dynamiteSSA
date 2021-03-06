package sexpr

import hlvm._


case class ExprNode(builder: Builder) {


  private def handleValueABranch(tt: ABranch): Value = {
    println("handleExpr: " + tt.cmd)
    tt match {
      case ABranch("If", List(cond, thenBr, ASymbol("Else"), elseBr)) =>
        IfElse(cond, thenBr, elseBr).handleExpr()

      case ABranch("Block", params) =>
        println("Block:")
        for (p <- params) {
          println("-> " + p)
        }
        for (i <- 0 until (params.size - 1)) {
          IRNode.handleAST(params(i))
        }
        handleValue(params.last)

      case ABranch("ArrayCreate", List(ty, count)) =>
        ArrayCreate(ty, count).handleExpr()

      case ABranch(fun: String, params: List[CodeTree]) =>
        Call(fun, params).handleExpr()
    }

  }


  def handleValue(tt: CodeTree): Value = {
    tt match {
      case xx: ABranch =>
        handleValueABranch(xx)
      case ANumber(n) =>
        ConstantLong(null, n)
      case ASymbol(ss) if ss(0) == '\"' =>
        // TODO Module.get(...)
        ConstantString(null, ss.substring(1, ss.length-1))


      case ASymbol("EmptyTree") =>
        null

      case ASymbol("null") =>
        //        PointerType* PointerTy_struct_Object = PointerType::getUnqual(TheModule->getTypeByName("struct.Object"));
        //        Value* res = ConstantPointerNull::get(PointerTy_struct_Object);
        null
      case ASymbol(xx) =>
        IRNode.NamedValues(xx)


    }
  }


  trait ExprNode {

    def handleExpr(): Value

  }


  case class IfElse(cond: CodeTree, thenBranch: CodeTree, elseBranch: CodeTree) {

    def handleExpr(): Value = {
      println("handleIfExpr: " + cond)

      println("thenBranch = " + thenBranch)
      println("elseBranch = " + elseBranch)
      println("-----")

      val condVal = handleValue(cond)

      val bb = builder.bb

      // Create blocks for the then and else cases.  Insert the 'then' block at the
      // end of the function.
      val thenBB = BasicBlock.newBasicBlock("then")
      val elseBB = BasicBlock.newBasicBlock("else")

      val retVal = builder.createAlloca(Int32Ty, "iftmp")
      val condBr: Cond = builder.createCondBr(condVal, thenBB, elseBB)

      // Emit then value.
      builder.setInsertPoint(thenBB)
      val thenVal = ExprNode(builder).handleValue(thenBranch)
      builder.createStore(thenVal, retVal)

      // Emit else block.
//      TheFunction->getBasicBlockList().push_back(ElseBB);
      builder.setInsertPoint(elseBB)
      val elseVal = ExprNode(builder).handleValue(elseBranch)
      builder.createStore(elseVal, retVal)

      builder.setInsertPoint(bb)

      retVal
    }

  }

  case class ArrayCreate(t: CodeTree, v: CodeTree) {
    def handleExpr(): Value = {
      val arraySize = handleValue(v)
      val arrayType = Type.getTypeOfName(t.asInstanceOf[ASymbol].value)
      builder.createAlloca(arrayType, arraySize, "ArrayCreate")
    }

  }

  case class Call(op: String, args: Seq[CodeTree]) {

  def handleExpr(): Value = {
    println("handleCall " + op + " : " + args.size)
    val calcArgs = args.map(handleValue)
    val func = IRNode.TheModule.getFunction(op)
    builder.createCall(func, calcArgs)

  }



  }




}

