package com.sootsafe.engine

import org.scalatest.{Matchers, WordSpecLike}

class BoverketTest extends WordSpecLike with Matchers with TestFixture {

  "Boverket engine" should {
    "calculate proper values from example" in {
      val result = Boverket.calculatePressureLoss(linkedModel)
      result should be (241.18325024548193)
    }

    "calculate proper values from example 2" in {
      val result = Boverket.calculatePressureLoss(linkedModel, 57)
      result should be (67.62744006969751)
    }
  }

}
