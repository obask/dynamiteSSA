import sexpr._
import sexpr.Parser._

object Enterprise {

  def makeAST(tokens: List[LToken], state: List[CodeTree]): (ABranch, List[LToken]) = {
    if (tokens.isEmpty) {
      (ABranch("result", state.reverse), tokens)
    } else {
      tokens.head match {
        case LeftParenthesis =>
          val tt = makeAST(tokens.tail, List())
          makeAST(tt._2, tt._1 :: state)
        case LInt(x) => makeAST(tokens.tail, ANumber(x) :: state)
        case LDouble(x) => makeAST(tokens.tail, ADouble(x) :: state)
        case LAtom(x) => makeAST(tokens.tail, ASymbol(x) :: state)
        case RightParenthesis =>
          val params = state.reverse
          //          println(params)
          (ABranch(params.head.toString, params.tail), tokens.tail)
      }
    }
  }

  def main(args: Array[String]): Unit = {

    println("Hello World")

  }

}
