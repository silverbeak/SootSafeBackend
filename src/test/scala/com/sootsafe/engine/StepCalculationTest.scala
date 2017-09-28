package com.sootsafe.engine

import java.io.InputStream

import com.sootsafe.model.{LinkedNode, Model, ModelBuilder}
import com.sootsafe.serializers.NodeSerializer
import com.sootsafe.valuetable.ValueResolver
import org.json4s.native.Serialization.read
import org.json4s.{DefaultFormats, Formats}
import org.scalatest.{Matchers, WordSpecLike}

class StepCalculationTest extends WordSpecLike with Matchers {

  implicit val formats: Formats = DefaultFormats + NodeSerializer

  private def createLinkedModel(fileName: String): LinkedNode = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    val defaultJson = scala.io.Source.fromInputStream(stream).mkString
    val model = read[Model](defaultJson)
    new ModelBuilder(model).buildModel()
  }

  "Step Calculation" must {

    "properly calculate the pressure from target node to the next junction" in {
      val linkedModel = createLinkedModel("/defaultTestData.json")
      val valueResolver: ValueResolver = new ValueResolver {}
      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(linkedModel)

      val pressure = StepCalculation.calculateFlowFromNodeToNextJunction(linkedModel.locateTargetNode(), pressureLossTable)

      pressure should be(2.400202446689589)
    }

    "properly calculate the pressure from one (random) node to the next junction" in {
      val linkedModel = createLinkedModel("/defaultTestData.json")
      val valueResolver: ValueResolver = new ValueResolver {}
      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(linkedModel)

      val startNode = linkedModel.findNextJunction(linkedModel.locateTargetNode())

      val pressure = StepCalculation.calculateFlowFromNodeToNextJunction(startNode, pressureLossTable)

      pressure should be(3.164158585861644)
    }

    "properly calculate the pressure from a random node to the next junction" in {
      val linkedModel = createLinkedModel("/defaultTestData.json")
      val valueResolver: ValueResolver = new ValueResolver {}
      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(linkedModel)

      val targetNode = linkedModel.locateTargetNode()
      val firstJunction = linkedModel.findNextJunction(targetNode)
      val secondJunction = linkedModel.findNextJunction(firstJunction)

      firstJunction should not be secondJunction

      val pressure = StepCalculation.calculateFlowFromNodeToNextJunction(secondJunction, pressureLossTable)

      pressure should be(11.486546343378132)
    }

  }
}
