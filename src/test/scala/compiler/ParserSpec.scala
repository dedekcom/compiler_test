package compiler

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParserSpec extends Specification {
  "ParserTree " should {
    "read functions " in {
      val parser = new Parser
      parser.execute("code2.dd")

      parser.funcs.keySet mustEqual Set("fibo", "main")
    }

    "read functions 2" in {
      val parser = new Parser
      parser.execute("code3.dd")

      parser.funcs.keySet mustEqual Set("fibo", "main")
    }

    "execute code 4" in {
      val parser = new Parser
      // no end if
      parser.execute("code4.dd") must throwA[Parser.ParserSyntaxErrorException]
    }

    "read code 5" in {
      val parser = new Parser
      parser.execute("code5.dd")

      parser.funcs.keySet mustEqual Set("fibo", "main", "fiboRec", "printfib")
    }
  }
}
