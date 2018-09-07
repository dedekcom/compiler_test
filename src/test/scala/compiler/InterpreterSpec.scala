package compiler

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class InterpreterSpec extends Specification {
  "ParserTree " should {
    "read functions " in {
      val interpreter = new Interpreter("code2.dd")

      interpreter.parser.funcs.keySet mustEqual Set("fibo", "main")
    }

    "read functions 2" in {
      val interpreter = new Interpreter("code3.dd")

      interpreter.parser.funcs.keySet mustEqual Set("fibo", "main")
    }

    "execute code 4" in {
      val interpreter = new Interpreter("code4.dd")
      // no end if
      interpreter.execute() must throwA[Parser.ParserSyntaxErrorException]
    }

    "read code 5" in {
      val interpreter = new Interpreter("code5.dd")

      interpreter.parser.funcs.keySet mustEqual Set("fibo", "main", "fiboRec", "printfib")
    }
  }
}
