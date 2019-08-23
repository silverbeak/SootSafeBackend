package com.sootsafe.model

import com.sootsafe.engine.TestFixture
import org.scalatest.{Matchers, WordSpecLike}

class LinkedNodeTest extends WordSpecLike with Matchers with TestFixture {

  "LinkedModel" must {
    "be able to generate lazy iterator over junctions" in {
      val iterator = linkedModel.iterateJunctions()
      iterator.toSeq.size should be(7)
    }
  }

}
