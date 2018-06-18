package compiler.eval

import scala.collection.mutable
import RPN.{OutRpn, RPNEvalException, RpnOp, RpnVar}

/**
  * Reverse Polish Notation
  */
object RPN {

  trait OutRpn
  case class RpnVar(vr: Any) extends Variable with OutRpn {
    set(vr)
  }
  case class RpnOp(op: String) extends OutRpn

  class RPNEvalException(msg: String) extends Exception(msg)

  val priorities: Map[String,Int] = Map("||" -> 1, "&&" -> 1,
    "==" -> 2, "!=" -> 2, ">=" -> 2, "<=" -> 2, "<" -> 2, ">" -> 2,
    "+" -> 3, "-" -> 3,
    "*" -> 4, "/" -> 4)
}

class RPN {

  var output: mutable.Queue[OutRpn] = mutable.Queue()
  var opstack: List[RpnOp] = List()

  var parenthesisCnt: Int = 0

  def containsParenthesis: Boolean = parenthesisCnt > 0

  def clear(): Unit = {
    output = mutable.Queue()
    opstack = List()
  }

  def nextVar(v: Any): Unit = {
    output += RpnVar(v)
  }

  def nonEmpty: Boolean = output.nonEmpty || opstack.nonEmpty

  def nextOp(op: String): Unit = op match {
    case "(" =>
      parenthesisCnt += 1
      pushOp("(")

    case ")" =>
      parenthesisCnt -= 1
      def pop(): Unit = opstack.head match {
        case RpnOp("(") => popOp()
        case o =>
          output += popOp()
          pop()
      }
      pop()

    case o => val prior = RPN.priorities(o)
        while(opstack.nonEmpty && (opstack.head.op != "(" && RPN.priorities(opstack.head.op) >= prior))
          output += popOp()
        pushOp(o)
  }

  private def pushOp(op: String): Unit = {
    opstack = RpnOp(op) :: opstack
  }

  private def popOp(): RpnOp = {
    val r = opstack.head
    opstack = opstack.tail
    r
  }

  private def flushOpStack(): Any = {
    while(opstack.nonEmpty)
      output += popOp()
  }

  def next(e: String): Unit = e match {
    case "(" | ")" => nextOp(e)
    case o if RPN.priorities.contains(o) => nextOp(o)
    case i if i.forall(_.isDigit) => nextVar(i.toInt)
    case r if r.matches("[0-9]+[\\.][0-9]+") => nextVar(r.toDouble)
    case s => nextVar(s)
  }

  def parse(equation: String): mutable.Queue[OutRpn] = {
    clear()
    equation.split(' ').foreach(next)
    flushOpStack()
    output
  }

  def eval(): Any = {
    flushOpStack()
    output.foldLeft(List[Variable]())(
      (list, token) => (list, token) match {
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("*"))  => y.mul(x) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("/"))  => y.div(x) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("+"))  => y.add(x) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("-"))  => y.sub(x) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("==")) => RpnVar(y.eq(x)) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("!=")) => RpnVar(y.neq(x)) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("<=")) => RpnVar(y.lte(x)) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp(">=")) => RpnVar(y.gte(x)) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("<"))  => RpnVar(y.lt(x)) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp(">"))  => RpnVar(y.gt(x)) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("&&")) => RpnVar(y.and(x)) :: tail
        case ( (x: RpnVar) :: (y: RpnVar) :: tail, RpnOp("||")) => RpnVar(y.or(x)) :: tail
        case ( _, v: RpnVar) => v :: list
        case _ => throw new RPNEvalException(s"token=$token, list=${list.toString()}")
      }).head.get
  }

}
