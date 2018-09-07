package compiler

import scala.util.matching.Regex
import Tokens._

import scala.annotation.tailrec
import scala.util.{Success, Failure, Try}

object Lexer {
  class LexerException(msg: String) extends Exception(msg)

  val CommentLinePat: Regex = "//.*".r
  val CommentStartPat: Regex = "/[*].*".r
  val CommentEndPat: Regex = "[*]/.*".r
  val CharDoubleQuotePat: Regex = "[\"].*".r
  val NumRealPat: Regex = "[0-9]+[\\.][0-9]+.*".r
  val NumIntPat: Regex = "[0-9]+.*".r
  val Operators = Set(".", ",", "=", "+", "-", "*", "/", "%", "!", "<", ">", "(", ")", "{", "}", ":",
    "==", "!=", "<=", ">=", "&&", "||")
  val SpecChars = Set('.', ',', '=', '+', '-', '*', '/', '%', '!', '<', '>', '(', ')', '{', '}', ':',
    '&', '|')
  val KeyWords = Map("if" -> KWIF, "else" -> KWELSE, "end" -> KWEND, "func" -> KWFUNC, "val"->KWVAL, "endif" -> KWENDIF)

  @tailrec
  private def _scan(txt: List[String], scanned: List[List[Token]]): Try[List[List[Token]]] = txt match {

    case List() =>
      val res = scanned.filter(_.nonEmpty).map(_.reverse).reverse
      Success(res)

    case h::tail => h match {

      case "" => _scan(tail, nextCodeLine(scanned))

      case CommentLinePat() => _scan(tail, nextCodeLine(scanned))

      case CommentStartPat() => _scan( scanComment( pop(h, "/*") :: tail ), scanned )

      case CharDoubleQuotePat() => scanLiteral( pop(h, "\""), "") match {
        case (s, literal) => _scan(s :: tail, pushToken(Literal(literal), scanned))
      }

      case any =>
        val (s, token) = any.charAt(0) match {
          case c if SpecChars.contains(c) => scanOperator(c.toString, any.substring(1))

          case n if n.isDigit => any match {
            case NumRealPat() => scanReal(n.toString, any.substring(1))
            case NumIntPat() => scanInt(n.toString, any.substring(1))
          }

          case pref if pref.toString.matches("[_a-zA-Z]") =>
            val (line, id) = scanIdent(pref.toString, any.substring(1))
            if (KeyWords.contains(id)) (line, KeyWords(id))
            else (line, Ident(id))

          case e if isEmptyChar(e) => (h.substring(1), Empty)
        }
        _scan(s :: tail, pushToken(token, scanned))
    }
  }

  private def nextCodeLine(code: List[List[Token]]): List[List[Token]] = List() :: code

  @tailrec
  private def scanInt(number: String, line: String): (String, NumInt) = line match {
    case "" => ("", NumInt( number ))
    case _ => line.charAt(0) match {
      case c if c.isDigit => scanInt(number + c, line.substring(1))

      case _ => (line, NumInt( number ))
    }
  }

  @tailrec
  private def scanIdent(id: String, line: String): (String, String) = line match {
    case "" => ("", id)
    case _ => line.charAt(0) match {
      case c if c.toString.matches("[_a-zA-Z0-9]") => scanIdent(id + c, line.substring(1))

      case _ => (line, id)
    }
  }

  @tailrec
  private def scanReal(number: String, line: String): (String, NumReal) = line match {
    case "" => ("", NumReal( number ))
    case _ => line.charAt(0) match {
      case c if c.isDigit => scanReal(number + c, line.substring(1))

      case dot if dot == '.' && !number.contains(".") => scanReal(number + dot, line.substring(1))

      case _ => (line, NumReal( number ))
    }
  }

  @tailrec
  private def scanOperator(op: String, line: String): (String, Op) = line match {
    case "" => ("", Op( validateOperator(op) ))
    case _ => line.charAt(0) match {
      case c if SpecChars.contains(c) && Operators.contains(op + c) => scanOperator(op + c, line.substring(1))

      case _ => (line, Op( validateOperator(op) ))
    }
  }

  private def validateOperator(op: String) = {
    if (Operators.contains(op)) op
    else throw new LexerException(s"wrong operator $op")
  }

  private def pushToken(token: Token, code: List[List[Token]]): List[List[Token]] = token match {
    case Empty => code
    case tk => code match {
        case List () => List (List (tk) )
        case h :: tail => (tk :: h) :: tail
      }
  }

  private def isEmptyChar(ch: Char) = ch == ' ' || ch == '\t' || ch == '\r'

  @tailrec
  def scanComment(txt: List[String]): List[String] = txt match {
    case List() => throw new LexerException("comment not closed")
    case h::tail => h match {
      case "" => scanComment( tail )
      case CommentEndPat() => pop(h, "*/") :: tail
      case _ => scanComment(h.substring(1) :: tail)
    }
  }

  @tailrec
  def scanLiteral(line: String, literal: String): (String, String) = line match {
    case "" => throw new LexerException("literal not closed")
    case CharDoubleQuotePat() => ( pop(line, "\""), literal)
    case _ =>
      val s = line.splitAt(1)
      scanLiteral(s._2, literal + s._1)
  }

  private def pop(txt: String, pattern: String): String = txt.substring(pattern.length)

  def scan(txt: List[String]): List[List[Token]] = {
    _scan(txt, List())  match {
      case Success(res) => res
      case Failure(ex) =>
        println(s"LexerException: ${ex.getMessage}")
        List()
    }
  }

  def tryScan(txt: List[String]): Try[List[List[Token]]] = {
    _scan(txt, List())
  }
}

