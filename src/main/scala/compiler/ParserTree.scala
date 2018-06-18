package compiler

import Tokens._
import compiler.eval.{Func, RPN, Variable}

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success}
import ParserTree._

object ParserTree {
  class ParserSyntaxErrorException(msg: String) extends Exception(msg)
  class ParserNoSuchFunctionException(msg: String) extends Exception(msg)
}

class ParserTree {
  var funcs: Map[String, Func] = Map()
  var code: IndexedSeq[List[Token]] = Array[List[Token]]()
  var varsStack: List[Variable] = List()

  val supportedOperators: Set[String] = Set("+", "-", "*", "/", "==", "!=", "<=", ">=",
    "<", ">", "||", "&&", "(", ")")

  /**
    * Run code from file
    * code must contain 'main' function without arguments
    * @param filename name of file with code
    */
  def execute(filename: String): Unit = execute(Source.fromResource(filename))

  def execute(src: BufferedSource): Unit = {
    val lines = SrcTransformer( src )
    Lexer.tryScan(lines) match {
      case Success(codeLines) =>
        execute(codeLines)

      case Failure(ex) =>
        println(s"Lexer Exception ${ex.getMessage}")
    }
  }

  def execute(lines: List[List[Token]]): Unit = {
    code = lines.toIndexedSeq
    funcs = code.foldLeft((Map[String, Func](), 0)) { (acc, el) => el match {
      case KWFUNC :: Ident(name) :: tail => (acc._1.updated(name, new Func(name, acc._2)), acc._2 + 1)
      case _ => (acc._1, acc._2 + 1)
    }
    }._1
    parseFunc("main")
  }

  private def parseFunc(funcName: String): Unit = {
    funcs.get(funcName) match {
      case Some(func) =>
        code(func.start) match {
          case KWFUNC :: Ident(name) :: args => scanFuncArgs(new Func(funcName, func.start), func.start, args)

          case line => throw new ParserSyntaxErrorException(s"parse func $line")
        }
      case None => throw new ParserNoSuchFunctionException(funcName)
    }
  }

  private def scanFuncArgs(func: Func,  idCodeLine: Int, codeLine: List[Token]): Unit = codeLine match {
    case Op("(") :: Op(")") :: Op(":") :: Nil => parseNextLine(func, func.start, List())
    case Op("(") :: args => scanFuncListArgs(func, idCodeLine, args)
    case rest => throw new ParserSyntaxErrorException(s"parse func args $rest" )
  }

  private def popVar(): Variable = {
    val v = varsStack.head
    varsStack = varsStack.tail
    v
  }

  private def pushVar(v: Any): Unit = v match {
    case variable: Variable => varsStack = variable :: varsStack
    case any => varsStack = new Variable().set(v) :: varsStack
  }

  private def scanFuncListArgs(func: Func,  idCodeLine: Int, codeLine: List[Token]): Unit = {
    def newVar(varName: String): Unit = {
      func.newVar(varName)
      func.pushVar(varName)
    }
    codeLine match {
      case Ident(arg) :: Op(")") :: Op(":") :: Nil =>
        newVar(arg)
        def popFuncVars(): Unit = func.popVar() match {
          case Some(v) =>
            v.set( popVar() )
            popFuncVars()
          case None =>
        }
        popFuncVars()
        parseNextLine(func, func.start, List())

      case Ident(arg) :: Op(",") :: args =>
        newVar(arg)
        scanFuncListArgs(func, idCodeLine, args)

      case rest => throw new ParserSyntaxErrorException(s"parse func args $rest" )
    }
  }

  private case class IfBlock(justCount: Boolean, evalIfOrElse: Boolean)

  private def parseNextLine(func: Func, idCodeLine: Int, ifs: List[IfBlock]): Unit = {
    if (idCodeLine+1 < code.length)
      parseBody(func, idCodeLine + 1, code(idCodeLine + 1), ifs)
    else
      throw new ParserSyntaxErrorException(s"missing keyword end in function: ${func.name}")
  }

  private def parseBody(func: Func, idCodeLine: Int, codeLine: List[Token], ifs: List[IfBlock]): Unit = {
    lazy val process: Boolean = ifs.isEmpty || (!ifs.head.justCount && ifs.head.evalIfOrElse)
    codeLine match {
      case KWVAL :: Ident(valName) :: Op("=") :: tail =>
        if (process) {
          parseExpr(func, tail, new RPN())
          func.newVar(valName)
          func.setVar(valName, popVar())
        }
        parseNextLine(func, idCodeLine, ifs)

      case Ident(valName) :: Op("=") :: tail =>
        if (process) {
          parseExpr(func, tail, new RPN())
          func.setVar(valName, popVar())
        }
        parseNextLine(func, idCodeLine, ifs)

      case KWIF :: tail =>
        if (process) {
          parseExpr(func, tail, new RPN()) match {
            case Op(":") :: Nil => popVar().get match {
              case false => parseNextLine(func, idCodeLine, IfBlock(justCount = false, evalIfOrElse = false) :: ifs)
              case _ => parseNextLine(func, idCodeLine, IfBlock(justCount = false, evalIfOrElse = true) :: ifs)
            }
            case any => throw new ParserSyntaxErrorException(s"wrong if statement $any")
          }
        } else {
          parseNextLine(func, idCodeLine, IfBlock(justCount = true, evalIfOrElse = false) :: ifs)
        }

      case KWELSE :: Op(":") :: Nil =>
        if (ifs.isEmpty)
          throw new ParserSyntaxErrorException(s"else without if")
        ifs.head match {
          case IfBlock(false, true) =>  parseNextLine(func, idCodeLine, IfBlock(justCount = false, evalIfOrElse = false) :: ifs.tail )

          case IfBlock(false, false) => parseNextLine(func, idCodeLine, IfBlock(justCount = false, evalIfOrElse = true) :: ifs.tail )

          case _ => parseNextLine(func, idCodeLine, IfBlock(justCount = true, evalIfOrElse = false) :: ifs.tail )
        }

      case KWENDIF :: Nil =>
        parseNextLine(func, idCodeLine, ifs.tail)

      case KWEND :: Nil =>
        if (ifs.nonEmpty)
          throw new ParserSyntaxErrorException("no end if")

      case anyList =>
        if (process)
          parseExpr(func, anyList, new RPN())
        parseNextLine(func, idCodeLine, ifs)
    }

  }

  private def evalRpn(rpn: RPN): Unit = {
    if (rpn.nonEmpty)
      pushVar(rpn.eval())
  }

  private def parseExpr(func: Func, codeLine: List[Token], rpn: RPN): List[Token] = {
    def proceed(restCodeLine: List[Token]) = parseExpr(func, restCodeLine, rpn)

    codeLine match {
      case Nil => evalRpn(rpn); Nil

      case Op(",") :: tail => evalRpn(rpn); codeLine

      case Op(")") :: tail =>
        if (rpn.containsParenthesis) {
          rpn.nextOp(")")
          parseExpr(func, tail, rpn)
        } else {
          evalRpn(rpn)
          codeLine
        }

      case Op(":") :: tail => evalRpn(rpn); codeLine

      case Ident(funcName) :: Op("(") :: tail =>
        val restCode = parseFuncArgs(func, codeLine.tail)
        funcName match {
          case "println" => println(popVar().get)
          case rest => parseFunc(rest)
        }
        proceed( restCode )

      case NumInt(i) :: tail => rpn.nextVar(i.toInt); proceed(tail)
      case NumReal(r) :: tail => rpn.nextVar(r.toDouble); proceed(tail)
      case Literal(s) :: tail => rpn.nextVar(s); proceed(tail)

      case Ident(name) :: tail => rpn.nextVar(func.getVar(name).get); proceed(tail)

      case Op(op) :: tail if supportedOperators.contains(op) => rpn.nextOp(op); proceed(tail)

      case any => throw new ParserSyntaxErrorException(s"parse expr $any")
    }
  }

  private def parseFuncArgs(func: Func, codeLine: List[Token]): List[Token] = codeLine match {
    case Op("(") :: Op(")") :: tail =>  codeLine.tail
    case Op("(") :: tail =>      parseFuncListArgs(func, tail)
    case rest => throw new ParserSyntaxErrorException(s"parse func args $rest" )
  }

  private def parseFuncListArgs(func: Func, codeLine: List[Token]): List[Token] = codeLine match {
    case Op(")") :: tail =>  codeLine
    case Op(",") :: tail =>  parseFuncListArgs(func, tail)
    case any =>
      val restCode = parseExpr(func, any, new RPN())
      parseFuncListArgs(func, restCode)
  }

}
