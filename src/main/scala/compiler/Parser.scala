package compiler

import Tokens._
import compiler.eval.{Func, RPN, Variable}

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}
import Parser.{ParserMissingKeyWordEndException, ParserNoSuchFunctionException, ParserSyntaxErrorException}

object Parser {
  class ParserSyntaxErrorException(msg: String) extends Exception(msg)
  class ParserNoSuchFunctionException(msg: String) extends Exception(msg)
  class ParserMissingKeyWordEndException(msg: String) extends Exception(msg)
}

class Parser {
  var funcs: Map[String, Func] = Map()
  var code: IndexedSeq[List[Token]] = Array[List[Token]]()
  var varsStack: List[Variable] = List()

  def parse(lines: List[List[Token]]): Unit = {
    code = lines.toIndexedSeq
    funcs = code.foldLeft((Map[String, Func](), 0)) { (acc, el) => el match {
        case KWFUNC :: Ident(name) :: tail => (acc._1.updated(name, new Func(name, acc._2)), acc._2 + 1)
        case _ => (acc._1, acc._2 + 1)
      }
    }._1
    parseFunc("main")
  }

  def parseFunc(funcName: String): Unit = {
    funcs.get(funcName) match {
      case Some(func) =>
        code(func.start) match {
          case KWFUNC :: Ident(name) :: Op("(") :: Op(")") :: Op(":") :: Nil => parseNextLine(func, func.start, List())

          case KWFUNC :: Ident(name) :: Op("(") :: tail => scanFuncArgs(func, tail, readArg = true)

          case line => throw new ParserSyntaxErrorException(s"parse func $line")
        }
      case None => throw new ParserNoSuchFunctionException(funcName)
    }
  }

  def popVar(): Variable = {
    val v = varsStack.head
    varsStack = varsStack.tail
    v
  }

  def pushVar(v: Any): Unit = v match {
    case variable: Variable => varsStack = variable :: varsStack
    case any => varsStack = new Variable(any) :: varsStack
  }

  def scanFuncArgs(func: Func, codeLine: List[Token], readArg: Boolean): Unit = codeLine match {
    case Op(")") :: Op(":") :: Nil if !readArg =>
      def popFuncVars(): Unit = func.popVar() match {
        case Some(v) =>
          v.set( popVar() )
          popFuncVars()
        case None =>
      }
      popFuncVars()
      parseNextLine(func, func.start, List())

    case Ident(arg) :: tail if readArg =>
      func.newVar(arg)
      func.pushVar(arg)
      scanFuncArgs(func, tail, readArg = false)

    case Op(",") :: tail if !readArg =>
      scanFuncArgs(func, tail, readArg = true)

    case any => throw new ParserSyntaxErrorException(s"scan func ${func.name} args $any" )
  }

  case class IfBlock(justCount: Boolean, evalIfOrElse: Boolean)

  def parseBody(func: Func, idCodeLine: Int, codeLine: List[Token], ifs: List[IfBlock]): Unit = {
    def process: Boolean = ifs.isEmpty || (!ifs.head.justCount && ifs.head.evalIfOrElse)
    codeLine match {
      case KWVAL :: Ident(valName) :: Op("=") :: tail =>
        if (process) {
          parseExpr(func, tail, readList = false, readIf = false, new RPN())
          func.newVar(valName)
          func.setVar(valName, popVar())
        }
        parseNextLine(func, idCodeLine, ifs)

      case Ident(valName) :: Op("=") :: tail =>
        if (process) {
          parseExpr(func, tail, readList = false, readIf = false, new RPN())
          func.setVar(valName, popVar())
        }
        parseNextLine(func, idCodeLine, ifs)

      case KWIF :: tail =>
        if (process) {
          parseExpr(func, tail, readList = false, readIf = true, new RPN()) match {
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
          parseExpr(func, anyList, readList = false, readIf = false, new RPN())
        parseNextLine(func, idCodeLine, ifs)
    }
  }

  def parseNextLine(func: Func, idCodeLine: Int, ifs: List[IfBlock]): Unit = {
    if (idCodeLine+1 < code.length)
      parseBody(func, idCodeLine + 1, code(idCodeLine + 1), ifs)
    else
      throw new ParserMissingKeyWordEndException(func.name)
  }

  def evalRpn(rpn: RPN): Unit = {
    if (rpn.nonEmpty)
      pushVar(rpn.eval())
  }

  def parseExpr(func: Func, codeLine: List[Token], readList: Boolean, readIf: Boolean, rpn: RPN): List[Token] = {
    def proceed(restCodeLine: List[Token]) = parseExpr(func, restCodeLine, readList, readIf, rpn)
    codeLine match {
      case Nil =>
        evalRpn(rpn)
        Nil

      case Op(",") :: tail if readList =>
        evalRpn(rpn)
        codeLine

      case oppc@Op(")") :: tail =>
        if (rpn.containsParenthesis) {
          rpn.nextOp(")")
          parseExpr(func, tail, readList, readIf, rpn)
        } else if (readList) {
          evalRpn(rpn)
          codeLine
        } else throw new ParserSyntaxErrorException(s"unexpected operator $oppc")


      case Op("(") :: tail =>
        rpn.nextOp("(")
        parseExpr(func, tail, readList, readIf, rpn)

      case Op(":") :: tail if readIf =>
        evalRpn(rpn)
        codeLine

      case Ident(funcName) :: Op("(") :: Op(")") :: tail =>
        parseFunc(funcName)
        tail

      case Ident(funcName) :: Op("(") :: tail =>
        val restCode = parseFuncArgs(func, tail, readArg = true)
        funcName match {
          case "println" => println(popVar().get)
          case rest =>      parseFunc(rest)
        }
        restCode

      case NumInt(i) :: tail =>
        rpn.nextVar(i.toInt)
        proceed(tail)

      case NumReal(r) :: tail =>
        rpn.nextVar(r.toDouble)
        proceed(tail)

      case Literal(s) :: tail =>
        rpn.nextVar(s)
        proceed(tail)

      case Ident(name) :: tail =>
        rpn.nextVar(func.getVar(name).get)
        proceed(tail)

      case Op("+") :: tail =>
        rpn.nextOp("+")
        proceed(tail)

      case Op("-") :: tail =>
        rpn.nextOp("-")
        proceed(tail)

      case Op("*") :: tail =>
        rpn.nextOp("*")
        proceed(tail)

      case Op("/") :: tail =>
        rpn.nextOp("/")
        proceed(tail)

      case Op("==") :: tail =>
        rpn.nextOp("==")
        proceed(tail)

      case Op("!=") :: tail =>
        rpn.nextOp("!=")
        proceed(tail)

      case Op(">=") :: tail =>
        rpn.nextOp(">=")
        proceed(tail)

      case Op(">") :: tail =>
        rpn.nextOp(">")
        proceed(tail)

      case Op("<=") :: tail =>
        rpn.nextOp("<=")
        proceed(tail)

      case Op("<") :: tail =>
        rpn.nextOp("<")
        proceed(tail)

      case Op("&&") :: tail =>
        rpn.nextOp("&&")
        proceed(tail)

      case Op("||") :: tail =>
        rpn.nextOp("||")
        proceed(tail)

      case any => throw new ParserSyntaxErrorException(s"parse expr $any" )
    }
  }

  def parseFuncArgs(func: Func, codeLine: List[Token], readArg: Boolean): List[Token] = codeLine match {
    case Op(")") :: tail if !readArg =>
      tail

    case Op(",") :: tail if !readArg =>
      parseFuncArgs(func, tail, readArg = true)

    case any if readArg =>
      val restCode = parseExpr(func, any, readList = true, readIf = false, new RPN())
      parseFuncArgs(func, restCode, readArg = false)

    case rest => throw new ParserSyntaxErrorException(s"parse func args $rest" )
  }

  def execute(src: BufferedSource): Unit = {
    val lines = SrcTransformer( src )
    Lexer.tryScan(lines) match {
      case Success(codeLines) =>
        parse(codeLines)

      case Failure(ex) =>
        println(s"Lexer Exception ${ex.getMessage}")
    }
  }

  def execute(filename: String): Unit = execute(Source.fromResource(filename))

}
