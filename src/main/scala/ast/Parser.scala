package ast

object Parser {


  sealed abstract class LToken
  case object LeftParenthesis extends LToken
  case object RightParenthesis extends LToken
  case class LInt(value: Int) extends LToken
  case class LDouble(value: Double) extends LToken
  case class LAtom(value: String) extends LToken


  def toSomeInt(ss: String):Option[Int] = {
    try {
      Some(ss.toInt)
    } catch {
      case e:Exception => None
    }
  }

  def toSomeDouble(ss: String):Option[Double] = {
    try {
      Some(ss.toDouble)
    } catch {
      case e:Exception => None
    }
  }


  def tokenize(ss: String): Array[LToken] = {
    val ss1 = ss.replace("(", " ( ").replace(")", " ) ")
    val tokens = Array(ss1) flatMap (_ split " ") flatMap (_ split "\n") filter (!_.isEmpty)
    for (ss <- tokens)
      yield ss match {
        case "(" => LeftParenthesis
        case ")" => RightParenthesis
        case x => toSomeInt(x).map(LInt).getOrElse(
          toSomeDouble(x).map(LDouble).getOrElse(
            LAtom(x)))
      }
  }


  def makeFullAST(tokens: List[LToken]): List[CodeTree] = {
    val (branch, rest) = makeAST(tokens, List())
    branch.params
  }


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
          (ABranch(params.head.toString, params.tail), tokens.tail)
      }
    }
  }

}
