package compiler

import compiler.Lexer._
import compiler.Tokens._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class LexerSpec extends Specification {

  "Lexer " should {

    "scan a comment " in {
      scanComment(List("/*abcd */efg")) mustEqual List("efg")
      scanComment(List("/*.....","")) must throwA[LexerException]
    }

    "scan literal" in {
      scanLiteral("zigzig\"abc", "") mustEqual ("abc", "zigzig")
    }

    "scan code 1" in {
      scan(List("/* test */\t\t\"string 1\"")) mustEqual List(List(Literal("string 1")))
      scan(List("/**//*"," test"," */\t\t\"string 1\"")) mustEqual List(List(Literal("string 1")))
    }

    "scan code comment line" in {
      scan(List("// test ","  \"string 1\"  ")) mustEqual List(List(Literal("string 1")))
      scan(List("//////// ///// ","  \"string 1\"  ")) mustEqual List(List(Literal("string 1")))
    }

    "scan code comment line end" in {
      scan(List("  \"string 1\"  // comment end","")) mustEqual List(List(Literal("string 1")))
    }

    "scan operators" in {
      scan(List(" = == ((  // comment end","")) mustEqual List(List(Op("="), Op("=="), Op("("), Op("(")))
    }

    "scan code 2" in {
      scan(List("val x=5", " func sort(list) { qsort1(list,list.size) 3.14+5.1!=8.2.5}")) mustEqual List(
        List(KWVAL, Ident("x"), Op("="), NumInt("5")),
        List(KWFUNC, Ident("sort"), Op("("), Ident("list"), Op(")"), Op("{"), Ident("qsort1"),
          Op("("), Ident("list"), Op(","), Ident("list"), Op("."), Ident("size"), Op(")"),
          NumReal("3.14"), Op("+"), NumReal("5.1"), Op("!="), NumReal("8.2"), Op("."), NumInt("5"),
          Op("}")
          )
      )
    }
  }

  "SrcTransformer " should {
    "load code" in {
      val lines = SrcTransformer( Source.fromResource("code1.dd") )
      Lexer.scan(lines) mustEqual List(
        List(KWFUNC, Ident("fibo"), Op("("), Ident("first"), Op(","), Ident("second"), Op(","),
          Ident("n"), Op(")"), Op(":")),
        List(KWIF, Ident("n"), Op("<="), NumInt("0"), Op(":")),
        List(Ident("first")),
        List(KWELSE, Op(":")),
        List(Ident("fibo"), Op("("), Ident("second"), Op(","), Ident("first"), Op("+"), Ident("second"),
          Op(","), Ident("n"), Op("-"), NumInt("1"), Op(")")),
        List(KWENDIF),
        List(KWEND)
        )
        /*func fibo(first, second, n):
            if n <= 0:
                first
            else:
                fibo(second, first+second, n-1)
            endif
        end*/
    }
  }
}
