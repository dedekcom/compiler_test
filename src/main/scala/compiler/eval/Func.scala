package compiler.eval


object Func {
  class FuncNoSuchVarException(msg: String) extends Exception(msg)
}

class Func(val name: String, val start: Int) {
  var vars: Map[String, Variable] = Map()
  var funArgs: List[Variable] = List()

  def setVar(name: String, value: Any): Unit = {
    getVar(name).set(value)
  }

  def newVar(name: String): Unit = {
    val v = new Variable(name)
    vars = vars.updated(name, v)
  }

  def pushVar(name: String): Unit = {
    funArgs = getVar(name) :: funArgs
  }

  def popVar(): Option[Variable] = funArgs.headOption match {
    case Some(v) =>
      funArgs = funArgs.tail
      Some(v)
    case None => None
  }

  def getVar(name: String): Variable = vars.get(name) match {
    case Some(v) => v
    case None => throw new Func.FuncNoSuchVarException(name)
  }

  override def toString: String = s"Func(start=${start.toString}, vars=$vars"
}
