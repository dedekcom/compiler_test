package compiler

import compiler.eval.RPN
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RPNSpec extends Specification {

  "RPN " should {

    "transform equasion " in {
      val rpn = new RPN()
      //  ((15 ÷ (7 − (1 + 1))) × 3) − (2 + (1 + 1))
      // 15 7 1 1 + − ÷ 3 × 2 1 1 + + −
      val eq = "( ( 15 / ( 7 - ( 1 + 1 ) ) ) * 3 ) - ( 2 + ( 1 + 1 ) )"
      val res = rpn.parse(eq).map {
        case v: RPN.RpnVar => v.get.toString
        case RPN.RpnOp(o) => o
      }.mkString(" ")
      res mustEqual "15 7 1 1 + - / 3 * 2 1 1 + + -"

      rpn.eval() mustEqual 5
    }

  }

}
