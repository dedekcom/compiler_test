package compiler

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParserTreeSpec extends Specification {
  "ParserTree " should {
    "read functions " in {
      val parser = new ParserTree
      parser.execute("code2.dd")

      parser.funcs.keySet mustEqual Set("fibo", "main")
    }

    "read functions 2" in {
      val parser = new ParserTree
      parser.execute("code3.dd")

      parser.funcs.keySet mustEqual Set("fibo", "main")
    }

    "execute code 4" in {
      val parser = new ParserTree
      // no end if
      parser.execute("code4.dd") must throwA[ParserTree.ParserSyntaxErrorException]
    }

    "read code 5" in {
      val parser = new ParserTree
      parser.execute("code5.dd")

      parser.funcs.keySet mustEqual Set("fibo", "main", "fiboRec", "printfib")
    }
  }
}
