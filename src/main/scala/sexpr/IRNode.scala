package sexpr

import hlvm._

import scala.collection.mutable


object IRNode {

  val TheModule = Module()
  val builder = Builder()
  var CurrentFunction: Function = null

  val NamedValues = mutable.Map[String, Value]()


  trait IRNode {

    def handleIR(): Unit


  }


  def handleAST(tree: CodeTree): Unit = {
    assert(tree.isInstanceOf[ABranch])
    tree match {
      case ABranch("DefDef", Seq(name: ASymbol, args: ABranch, body: CodeTree)) =>
        DefDef(name, args, body).handleIR()

      case ABranch("ValDef", List(varName: ASymbol, valueX: CodeTree)) =>
        ValDef(varName, valueX).handleIR()

      case ABranch("ArrayAssign", List(varName: ASymbol, posX: CodeTree, valueX: CodeTree)) =>
        ArrayAssign(varName, posX, valueX).handleIR()

      case ABranch(cmd: String, params) =>
        cmd match {
          case "Prototype" =>
            val name = params.head.asInstanceOf[ASymbol].value
            assert("#" == params(1).asInstanceOf[ABranch].cmd)
            val args = params(1).asInstanceOf[ABranch].params
            assert("->" == params(2).asInstanceOf[ASymbol].value)
            val ret = params(3)
            Prototype(name, args, ret).handleIR()
          case "TypeDef" => ???
          case "TypeFFI" => ???
          case "ArrayAssign" => ???
          case _ =>
            println("OLOLO")
            ExprNode(builder).handleValue(tree)
        }

    }
  }


  case class ValDef(varName: ASymbol, valueX: CodeTree) extends IRNode {
    def handleIR(): Unit = {
      println("handleValDef: " + varName)
      val calcResult = ExprNode(builder).handleValue(valueX)
      val ptrContext = builder.createAlloca(calcResult.getType, varName.value)

      // Add arguments to variable symbol table.
      // FIXME should shadow up to stack vars
      NamedValues(varName.value) = ptrContext

      // Store the initial value into the alloca.
      builder.createStore(calcResult, ptrContext)


    }

  }


  case class Prototype(name: String, fields: Seq[CodeTree], returnType: CodeTree) {

    def handleIR(): Unit = {
      println("handleSignature " + name + " -> " + returnType)
      val objectTypePtr = Type.getPtrToOpaqueTypes(returnType)
      val args = fields.map(Type.getPtrToOpaqueTypes)

      val ft = FunctionType(objectTypePtr, args)
      val theFunction = TheModule.functionCreate(ft, externalLinkage = true, name)
      assert(theFunction != null)

    }

  }

  case class DefDef(name: ASymbol, args: ABranch, body: CodeTree) extends IRNode {
    def handleIR(): Unit = {
      println("def handleDefDef " + name + " " + args.params.size)

//      val ptr2Object = Type.LLVMPointerType(TheModule.getTypeByName("struct.Object"))
//      val proto = args.params.map(Type.getPtrToOpaqueTypes)
//      val ft = FunctionType(ptr2Object, proto)
//      val theFunctionX = TheModule.functionCreate(ft, externalLinkage = true, name.value)
      // Create a new basic block to start insertion into.
      val bb = BasicBlock.newBasicBlock("entry")
      val theFunctionX = TheModule.functionSetBody(name.value, Seq(), bb)
      builder.setInsertPoint(bb)
      //
      CurrentFunction = theFunctionX
      val ret = ExprNode(builder).handleValue(body)
      //    RetVal->dump();
      if (ret != null) {
        builder.createRet(ret)
        //      // Validate the generated code, checking for consistency.
        //      verifyFunctionX(*theFunctionX);
        //
        //      // Optimize the FuctionX.
        //
        //      //        TheFPM->run(*theFunctionX);
        //        theFunctionX
        Unit
      } else {
        null
      }
      CurrentFunction = null

    }


  }

  case class ArrayAssign(varNameX: ASymbol, posX: CodeTree, valueX: CodeTree) {


    def handleIR() = {
      val varName = NamedValues(varNameX.value)
      val pos = ExprNode(builder).handleValue(posX)
      val value = ExprNode(builder).handleValue(valueX)

      val ptr_24 = builder.createGEP(varName, Array(pos))
      val void_25 = builder.createStore(value, ptr_24)
      void_25
    }

  }

}
