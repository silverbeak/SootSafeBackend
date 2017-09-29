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

      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode.get
      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val secondJunction = firstJunction.findNextJunction().thisNode

      val pressure = StepCalculation.calculateFlowFromNodeToNextJunction(secondJunction, pressureLossTable)

      pressure should be(3.164158585861644)
    }

    "properly calculate the pressure from first junction to the next" in {
      val linkedModel = createLinkedModel("/defaultTestData.json")
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode().get
      val firstJunction = fireNode.findNextJunction().thisNode.get

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val pressure = StepCalculation.calculateFlowFromNodeToNextJunction(firstJunction.parent, pressureLossTable)

      pressure should be(2.400202446689589)
    }

    "properly traverse through a chain" in {
      val linkedModel = createLinkedModel("/defaultTestData.json")
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode().get
      val firstJunction = fireNode.findNextJunction().thisNode

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction.get, outletNode.get)

      var pressure: Double = 0
      var junction: Option[LinkedNode] = firstJunction.get.parent
      while(junction.nonEmpty) {
        pressure += StepCalculation.calculateFlowFromNodeToNextJunction(junction, pressureLossTable)
        junction = junction.get.findNextJunction().thisNode
      }

      pressure should be(54.36596993157699)
    }
  }
}
