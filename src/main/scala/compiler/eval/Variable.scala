package compiler.eval


object Variable {

  class VariableNotSupportedTypeException(msg: String) extends Exception(msg)
  class VariableNotSupportedOperationException(msg: String) extends Exception(msg)

}

class Variable {
  var variable: Any = _

  def set(v: Any): Variable = v match {
    case vr: Variable => set(vr.get)
    case any =>
      variable = any
      this
  }

  def get: Any = variable

  def getBool: Boolean = variable match {
    case b: Boolean => b
    case any => throw new Variable.VariableNotSupportedTypeException("getBool: " + toString)
  }

  def getInt: Int = variable match {
    case i: Int => i
    case d: Double => d.toInt
    case s: String => s.toInt
    case any => throw new Variable.VariableNotSupportedTypeException("getInt: " + toString)
  }

  def getReal: Double = variable match {
    case i: Int => i.toDouble
    case d: Double => d
    case s: String => s.toDouble
    case any => throw new Variable.VariableNotSupportedTypeException("getReal: " + toString)
  }

  def getString: String = variable.toString

  def add(v: Variable): Variable = variable match {
    case i: Int => set(i + v.getInt)
    case d: Double => set(d + v.getReal)
    case s: String => set(s + v.getString)
    case any => throw new Variable.VariableNotSupportedTypeException(toString)
  }

  def sub(v: Variable): Variable = variable match {
    case i: Int => set(i - v.getInt)
    case d: Double => set(d - v.getReal)
    case s: String => throw new Variable.VariableNotSupportedOperationException("sub string "+toString)
    case any => throw new Variable.VariableNotSupportedTypeException("sub: " + toString)
  }

  def mul(v: Variable): Variable = variable match {
    case i: Int => set(i * v.getInt)
    case d: Double => set(d * v.getReal)
    case s: String => throw new Variable.VariableNotSupportedOperationException("mul string "+toString)
    case any => throw new Variable.VariableNotSupportedTypeException("mul: " + toString)
  }

  def div(v: Variable): Variable = variable match {
    case i: Int => set(i / v.getInt)
    case d: Double => set(d / v.getReal)
    case s: String => throw new Variable.VariableNotSupportedOperationException("div string "+toString)
    case any => throw new Variable.VariableNotSupportedTypeException("div: " + toString)
  }

  def eq(v: Variable): Boolean = variable == v.get
  def neq(v: Variable): Boolean = variable != v.get

  def gt(v: Variable): Boolean = variable match {
    case i: Int => i > v.getReal
    case d: Double => d > v.getReal
    case s: String => throw new Variable.VariableNotSupportedOperationException("gt string "+toString)
    case any => throw new Variable.VariableNotSupportedTypeException(toString)
  }
  def gte(v: Variable): Boolean = eq(v) || gt(v)
  def lt(v: Variable): Boolean = !gte(v)
  def lte(v: Variable): Boolean = !gt(v)

  def and(v: Variable): Boolean = getBool && v.getBool
  def or(v: Variable): Boolean = getBool || v.getBool

  override def toString: String = s"Variable(variable=$variable)"
}

