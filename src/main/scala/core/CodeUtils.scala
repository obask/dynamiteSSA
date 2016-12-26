package core

object CodeUtils {

  def createCaseClass[T, T1](o: T1, args: Seq[Any]): T = {
    println("createCaseClass: " + o.getClass.getName)
    println("args " + args.length.toString + ": " + args.map(_.toString.take(100)).mkString(" |#| "))

    val tmp = args map { _.asInstanceOf[AnyRef] }
    //    println(o.getClass.getMethods.toSeq.mkString("\n"))
    o.getClass.getMethods
      .find(x => x.getName == "apply" && !x.isBridge)
      .get.invoke(o, tmp: _*)
      .asInstanceOf[T]
  }

  //  def createCaseClass2[T, T1](o: T1, args: Seq[Any]): T = {
  //    println("createCaseClass: " + o.getClass.getName)
  //    println("args " + args.length.toString + ": " + args.map(_.toString.take(100)).mkString(" |#| "))
  //    val tmp = args map { _.asInstanceOf[AnyRef] }
  //    //    println(Ident.getClass.getMethods.toSeq.mkString("\n"))
  //    // TODO check isBridge or notIsBridge
  //    o.getClass.getMethods
  //      .find(x => x.getName == "apply" && x.isBridge)
  //      .get.invoke(o, tmp: _*)
  //      .asInstanceOf[T]
  //  }

}
