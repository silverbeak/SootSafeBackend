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
  private val linkedModel = createLinkedModel("/defaultTestData.json")

  private def createLinkedModel(fileName: String): LinkedNode = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    val defaultJson = scala.io.Source.fromInputStream(stream).mkString
    val model = read[Model](defaultJson)
    new ModelBuilder(model).buildModel()
  }

  "Step Calculation" must {

    "properly calculate the resistance from target node to the next junction" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode.get
      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val secondJunction = firstJunction.findNextJunction().thisNode

      val pressure = StepCalculation.calculateResistanceFromNodeToNextJunction(secondJunction, pressureLossTable)

      pressure should be(3.164158585861644)
    }

    "properly calculate the resistance from first junction to the next" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode().get
      val firstJunction = fireNode.findNextJunction().thisNode.get

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val pressure = StepCalculation.calculateResistanceFromNodeToNextJunction(firstJunction.parent, pressureLossTable)

      pressure should be(2.400202446689589)
    }

    "properly traverse to calculate resistance through a chain" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode().get
      val firstJunction = fireNode.findNextJunction().thisNode

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction.get, outletNode.get)

      var pressure: Double = 0
      var junction: Option[LinkedNode] = firstJunction.get.parent
      while(junction.nonEmpty) {
        pressure += StepCalculation.calculateResistanceFromNodeToNextJunction(junction, pressureLossTable)
        junction = junction.get.findNextJunction().thisNode
      }

      pressure should be(54.36596993157699)
    }

    "calculate flow from fire node to first junction" in {
      val fireNode = linkedModel.locateTargetNode()
      val flow = StepCalculation.calculateFlowFromNodeToNextJunction(fireNode)

      flow should be(17.0)
    }

    "calculate flow at pressure difference" in {
      val fireNode = linkedModel.locateTargetNode()
      val fireFlow = StepCalculation.calculateFlowAtPressureDifference(fireNode, 1000, 22)

      fireFlow should be (114.61397661875115)
    }
  }
}
