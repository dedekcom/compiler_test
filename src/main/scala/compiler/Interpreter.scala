package compiler

import compiler.Tokens.{Ident, KWFUNC}
import compiler.eval.Func

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success}

/**
  * Run code from file
  * code must contain 'main' function without arguments
  * @param filename name of file with code
  */

class Interpreter(filename: String) {
  val src: BufferedSource = Source.fromResource(filename)
  val lines = SrcTransformer( src )
  val parser: Parser = createParser

  private def createParser: Parser = Lexer.tryScan(lines) match {
    case Success(codeLines) =>
      val code = codeLines.toIndexedSeq
      val funcs = code.foldLeft((Map[String, Func](), 0)) { (acc, el) =>
        el match {
          case KWFUNC :: Ident(name) :: tail => (acc._1.updated(name, new Func(name, acc._2)), acc._2 + 1)
          case _ => (acc._1, acc._2 + 1)
        }
      }._1
      new Parser(code, funcs)

    case Failure(ex) =>
      throw ex
  }

  def execute(): Unit = {
     parser.execute()
  }

}
