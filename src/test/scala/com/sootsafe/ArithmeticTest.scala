package com.sootsafe

import com.sootsafe.arithmetic._
import org.scalatest.{Matchers, WordSpecLike}

class ArithmeticTest extends WordSpecLike with Matchers {

  "Arithmetic Texify" must {
    "display proper latex" in {
      val e1 = Value(33)
      val e2 = Value(2.0)

      val m1 = Multiplication(e1, e2)

      val r1 = Sqrt(m1)

      Arithmetic.texify(r1) should include("""\ \sqrt{33.0 \times 2.0}""")
    }
  }

  "Arithmetic Calculate" must {
    "calculate various stuff" in {
      val value1 = Value(450)
      val value2 = Value(550)

      val added = Addition(value1, value2)

      added.calculate() should be(1000)
    }
  }
}
