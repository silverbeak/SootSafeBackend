package com.sootsafe.engine

import java.io.InputStream

import com.sootsafe.model.{Model, ModelBuilder}
import com.sootsafe.serializers.NodeSerializer
import com.sootsafe.valuetable.ValueResolver
import org.json4s.native.Serialization.read
import org.json4s.{DefaultFormats, Formats}
import org.scalatest.{Matchers, WordSpecLike}

class PressureLossTest extends WordSpecLike with Matchers {

  implicit val formats: Formats = DefaultFormats + NodeSerializer

  "PressureLossTest" must {

    "calculatePressureLoss" in {
      val stream: InputStream = getClass.getResourceAsStream("/defaultTestData.json")
      val defaultJson = scala.io.Source.fromInputStream(stream).mkString

      val valueResolver: ValueResolver = new ValueResolver {}

      val model = read[Model](defaultJson)
      val linkedModel = new ModelBuilder(model).buildModel()

      val fireNode = linkedModel.locateTargetNode().get
      val outletNode = linkedModel.locateOutletNode()
      val firstJunction = fireNode.findNextJunction().thisNode.get

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)
      val pressureLoss = pressureLossTable.foldLeft(0d)((agg, pl) => pl.pressureLoss + agg)
      pressureLoss should be(54.365969931577)
      pressureLossTable.length should be(13)
    }

  }
}
