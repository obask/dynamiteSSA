package goast

object Tokens {

  sealed abstract class Token

  case object STRING extends Token
  case object IDENT extends Token
  case object INT extends Token
  case object FLOAT extends Token
  case object IMAG extends Token
  case object CHAR extends Token
  case class Wrapper(value: String) extends Token {
    override def toString: String = value.toString
  }

  val OTHERS = Set(
    "+",
    "-",
    "*",
    "/",
    "%",
    "&",
    "|",
    "^",
    "<<",
    ">>",
    "&^",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "&=",
    "|=",
    "^=",
    "<<=",
    ">>=",
    "&^=",
    "&&",
    "||",
    "<-",
    "++",
    "--",
    "==",
    "<",
    ">",
    "=",
    "!",
    "!=",
    "<=",
    ">=",
    ":=",
    "...",
    "(",
    "[",
    "{",
    ",",
    ".",
    ")",
    "]",
    "}",
    ";",
    ":",
    "break",
    "case",
    "chan",
    "const",
    "continue",
    "default",
    "defer",
    "else",
    "fallthrough",
    "for",
    "func",
    "go",
    "goto",
    "if",
    "import",
    "interface",
    "map",
    "package",
    "range",
    "return",
    "select",
    "struct",
    "switch",
    "type",
    "var"
  )

}
