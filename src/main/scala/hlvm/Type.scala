package hlvm

import ast.{ASymbol, CodeTree, ABranch}


object Type {




  def LLVMPointerType(x: Type): Type = null

  def getTypeOfName(typeName: String): Type = {
    //    if (typeName == "EmptyTree") {
    //      return Type::getVoidTy(C);
    //    }
    //    if (typeName == "String") {
    //      return PointerType::getUnqual(IntegerType::getInt8Ty(C));
    //    }
    //    if (typeName == "Int") {
    //      return IntegerType::getInt32Ty(C);
    //    }
    //    if (typeName == "Long") {
    //      return IntegerType::getInt64Ty(C);
    //    }
    //    if (typeName == "Boolean") {
    //      return IntegerType::getInt32Ty(C);
    //    }
    //    StructType *pType = TheModule->getTypeByName("struct." + typeName);
    //    if (!pType) {
    //      pType = StructType::create(TheModule->getContext(), "struct." + typeName);
    //    }
    //    assert(pType);
    //    return PointerType::getUnqual(pType);
    null
  }


  //  Type *getPtrToOpaqueTypes(SyntaxTreeP typeNameX) {
  //    // array types
  //    if (typeid(*typeNameX) == typeid(ABranch) && typeNameX->elemAt(0)->getString() == "Array") {
  //      auto typeName = typeNameX->elemAt(1)->getString();
  //      Type *typeOfName = getTypeOfName(typeName);
  //      return PointerType::getUnqual(typeOfName);
  //    }
  //    // else simple type
  //    auto typeName = typeNameX->getString();
  //    return getTypeOfName(typeName);
  //  }

  def getPtrToOpaqueTypes(typeName: CodeTree): Type = {
    typeName match {
      case ABranch("Array", List(typeArg)) =>
        val x: Type = getTypeOfName(typeArg.toString)
        LLVMPointerType(x)
      case tp: ASymbol =>
        getTypeOfName(tp.value)
    }


  }

  def getInt32Ty: Type = null


  def getFunctionType(ret: Type, args: Seq[Type], b: Boolean): Type = null



}

case class Type()


