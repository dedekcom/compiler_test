package compiler

object Tokens {

  abstract class Token(val value: String)

  case object Empty extends Token("")

  case class NumInt(v: String) extends Token(v)
  case class NumReal(v: String) extends Token(v)

  case class Literal(v: String) extends Token(v)

  case class Ident(v: String) extends Token(v)

  case class Op(v: String) extends Token(v)

  case object KWIF extends Token("if")
  case object KWELIF extends Token("elif")
  case object KWELSE extends Token("else")
  case object KWENDIF extends Token("endif")
  case object KWEND extends Token("end")
  case object KWFUNC extends Token("func")
  case object KWVAL extends Token("val")

}

