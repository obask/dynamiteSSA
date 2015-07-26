
import ast._
import hlvm._

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.io.Source


object Hello {

//  type State = mutable.HashMap[String, ALeaf]




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

//  Function *handleSignature(const string &name, const vector<SyntaxTreeP> &fields, SyntaxTreeP returnType) {
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
//    Function *theFunctionX = Function::Create(FT, Function::ExternalLinkage, name, TheModule);
//    assert(theFunctionX);
//
//    cerr << "-- theFunctionX" << endl;
//    //    theFunctionX->dump();
//    cerr << endl;
//    cerr.flush();
//
//    return theFunctionX;
//  }




  val programFile = Source.fromURL(getClass.getResource("/code.ir"))

  val myLispProgram = programFile.getLines().mkString("\n")

  def main(args: Array[String]) {
    val tokens = Parser.tokenize(myLispProgram)
    val tree = Parser.makeFullAST(tokens.toList)
    for (t1 <- tree) {
      println("PROCEED: " + t1.asInstanceOf[ABranch].cmd)
      IRNode.handleAST(t1)
    }

    for (xx <- IRNode.TheModule.getFunction("main1").bb.code) {
      xx.dump()
    }

  }

}


