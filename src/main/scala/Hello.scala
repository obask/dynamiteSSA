
import ast._
import hlvm._

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.io.Source


object Hello {

//  type State = mutable.HashMap[String, ALeaf]

  val TheModule = Module()


//  if (cmd == "Block") {
//  if (cmd == "Assign") {
//  if (cmd == "If") {
//  if (cmd == "SelectApply") {
//  if (cmd == "Apply") {
//  if (cmd == "ArrayCreate") {

//  // other ABranch
//  vector<SyntaxTreeP> argsX;
//  int sz = params.size();
//  for (size_t i = 1; i < sz; ++i) {
//    argsX.push_back(params.at(i));
//  }
//  return handleCall(cmd, argsX);

  def handleValueABranch(cmd: String, params: Seq[CodeTree]): Value = {
    cmd match {
      case "Block" =>
        println("Block:")
        for (p <- params) {
          println("-> " + p)
        }
        for (i <- 0 until (params.size - 1)) {
          handleIR(params(i))
        }
        handleValue(params.last)



    }

    null
  }


//  FunctionX *handleSignature(const string &name, const vector<SyntaxTreeP> &fields, SyntaxTreeP returnType) {
//    LLVMContext &C = getGlobalContext();
//    cout << "handleSignature " << name << " -> " << returnType << endl;
//
//    Type *PointerTy_Object = getPtrToOpaqueTypes(returnType);
//
//    vector<string> args;
//
//    cout << "DBG 2" << endl;
//
//    std::vector<Type *> argsTypes;
//    for (const auto &argType: fields) {
//      auto tmp = getPtrToOpaqueTypes(argType);
//      argsTypes.push_back(tmp);
//    }
//
//    FunctionXType *FT = FunctionXType::get(PointerTy_Object, argsTypes, false);
//    assert(FT);
//
//    //    FunctionXType *FT = FunctionXType::get(Type::getInt32Ty(C), {}, false);
//    FunctionX *theFunctionX = FunctionX::Create(FT, FunctionX::ExternalLinkage, name, TheModule);
//    assert(theFunctionX);
//
//    cerr << "-- theFunctionX" << endl;
//    //    theFunctionX->dump();
//    cerr << endl;
//    cerr.flush();
//
//    return theFunctionX;
//  }


  def FunctionCreate(ft: Type, externalLinkage: Boolean, name: String, module: Module): FunctionX = null


  def handleSignature(name: String, fields: Seq[CodeTree], returnType: CodeTree): FunctionX = {
    return null

    println("handleSignature " + name + " -> " + returnType)
    val objectTypePtr = Type.getPtrToOpaqueTypes(returnType)
    val args = fields.map(Type.getPtrToOpaqueTypes)

    val ft = Type.getFunctionType(objectTypePtr, args, b = false)
    val theFunction = FunctionCreate(ft, externalLinkage = true, name, TheModule)
    assert(theFunction != null)
    theFunction
  }


  def handleValueABRanch(): Value = {
    null
  }

  def handleValue(tt: CodeTree): Value = {
    tt match {
      case ABranch() =>
        handleValueABRanch()
      case ASymbol("EmptyTree") =>
        null
      case ASymbol("null") =>
//        PointerType* PointerTy_struct_Object = PointerType::getUnqual(TheModule->getTypeByName("struct.Object"));
//        Value* res = ConstantPointerNull::get(PointerTy_struct_Object);
        null



    }


    null
  }



  def handleDefDef(name: String, args: ABranch, body: ABranch): FunctionX = {
    println("def handleDefDef " + name + " " + args.params.size)
    val ptr2Object = Type.LLVMPointerType(TheModule.getTypeByName("struct.Object"))
    // TODO compex type not works
    // TODO move copy paste code to handlePrototype
    val proto = args.params.map(Type.getPtrToOpaqueTypes)
    val ft = Type.getFunctionType(ptr2Object, proto, b = false)
    val theFunctionX = FunctionCreate(ft, externalLinkage = true, name, TheModule)
    println("BODY:")
    for (instr <- body.params) {
      println("-> " + instr)
    }
    val builder = Builder()
    // Create a new basic block to start insertion into.
    val bb = BasicBlock("entry", theFunctionX)
    builder.setInsertPoint(bb)

    //
    val ret = handleValue(body)
    //    RetVal->dump();
    if (ret != null) {
      builder.createRet(ret)
      //      // Validate the generated code, checking for consistency.
      //      verifyFunctionX(*theFunctionX);
      //
      //      // Optimize the FunctionX.
      //
      //      //        TheFPM->run(*theFunctionX);
      theFunctionX
    } else {
      null
    }
  }



    def handleIR(tree: CodeTree): ALeaf = {
    tree match {
      case ABranch(cmd, params) =>
        cmd match {
          case "Signature" =>
            val name = params.head.asInstanceOf[ASymbol].value
            assert("#" == params(1).asInstanceOf[ABranch].cmd)
            val args = params(1).asInstanceOf[ABranch].params
            assert("->" == params(2).asInstanceOf[ASymbol].value)
            val ret = params(3)
            handleSignature(name, args, ret)
          case "DefDef" =>
            val name = params.head.asInstanceOf[ASymbol].value
            val args = params(1).asInstanceOf[ABranch]
            val body = params(2).asInstanceOf[ABranch]
            handleDefDef(name, args, body)

          case "TypeDef" =>
          case "TypeFFI" =>
          case "ArrayAssign" =>
          case "ValDef" =>
          case _ =>
            handleValueABranch(cmd, params)
        }
      case _ => ???
    }
    null
  }




  val programFile = Source.fromURL(getClass.getResource("/code.ir"))

  val myLispProgram = programFile.getLines().mkString("\n")

  def main(args: Array[String]) {
    val tokens = Parser.tokenize(myLispProgram)
    val tree = Parser.makeFullAST(tokens.toList)
    for (t1 <- tree) {
      println("PROCEED: " + t1.asInstanceOf[ABranch].cmd)
      handleIR(t1)
    }

  }

}


