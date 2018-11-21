package com.sootsafe.engine

import java.io.InputStream

import com.sootsafe.model._
import com.sootsafe.serializers.NodeSerializer
import com.sootsafe.valuetable.FakeValueResolver
import org.json4s.native.Serialization.read
import org.json4s.{DefaultFormats, Formats}
import org.scalatest.{Matchers, WordSpecLike}

class PressureLossTest extends WordSpecLike with Matchers {

  implicit val formats: Formats = DefaultFormats + NodeSerializer

  "PressureLossTest" must {

    "calculatePressureLoss" in {
      val valueResolver = FakeValueResolver
      val model = readModel("/defaultTestData.json")

      new ModelBuilder(model).buildModel() match {
        case Right(message) => fail(s"Expected model. Got error message: $message")
        case Left(linkedModel) =>
          val (pressureLossTable: Seq[PressureLossEntry], pressureLoss: Double) = aggregatePressureLoss(valueResolver, linkedModel)
          pressureLoss should be(55.544371375939654)
//          pressureLoss should be(54.64488178026986)
          pressureLossTable.length should be(13)
      }

    }

    "pick supplied pressure loss before calculated one" in {
      val model = readModel("/defaultTestData.json")
      val valueResolver = FakeValueResolver

      // Here, we take out a single pipe from the model extracted from json,
      // replace it with one that has a pressureloss value, and make sure that that value is being used
      val specificNode = model.nodeDataArray.find(_.key == 8).get
      val replacementPipe = Pipe.apply(specificNode.key, specificNode.ssInfo.copy(pressureloss = Some(33.3)))
      val newNodeDataArray = model.nodeDataArray.filterNot(_.key == 8) :+ replacementPipe
      val newModel = model.copy(nodeDataArray = newNodeDataArray)

      new ModelBuilder(newModel).buildModel() match {
        case Right(message) => fail(s"Expected model. Got error message: $message")
        case Left(linkedModel) =>
          val (pressureLossTable: Seq[PressureLossEntry], pressureLoss: Double) = aggregatePressureLoss(valueResolver, linkedModel)

          // This is where the new value should have changed
          pressureLoss should be(87.22437137593965)
          // Still the same length of the array, though
          pressureLossTable.length should be(13)
      }
    }

  }

  private def aggregatePressureLoss(valueResolver: FakeValueResolver.type, linkedModel: LinkedNode) = {
    val fireNode = linkedModel.locateTargetNode().get
    val outletNode = linkedModel.locateOutletNode()
    val firstJunction = fireNode.findNextJunction().thisNode.get

    val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)
    val pressureLoss = pressureLossTable.foldLeft(0d)((agg, pl) => pl.pressureLoss + agg)
    (pressureLossTable, pressureLoss)
  }

  private def readModel(fileName: String): Model = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    val defaultJson = scala.io.Source.fromInputStream(stream).mkString


    read[Model](defaultJson)
  }
}
