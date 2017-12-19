package com.sootsafe.engine

import org.scalatest.{Matchers, WordSpecLike}

class BoverketTest extends WordSpecLike with Matchers with TestFixture {

  "Boverket engine" should {
    "calculate proper values from example" in {
      Boverket.calculatePressureLoss(linkedModel, initialFirePressure = 1000d) match {
        case Left(result) =>
          result.size should be(6)
          FlowAndPressureSequence.aggregatePressure(result) should be (241.18325024548193)

        case Right(error) =>
          fail(s"Expected successful calculation. Got: $error")
      }
    }

    "calculate proper values from example 2" in {
      Boverket.calculatePressureLoss(linkedModel, Some(57d), 1000) match {
        case Left(result) =>
          result.size should be(6)
          FlowAndPressureSequence.aggregatePressure(result) should be (67.62744006969751)

        case Right(error) =>
          fail(s"Expected successful calculation. Got: $error")
      }
    }
  }

}
