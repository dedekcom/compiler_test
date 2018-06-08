package compiler


import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParserSpec extends Specification {
  "Parser " should {
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
  }
}
