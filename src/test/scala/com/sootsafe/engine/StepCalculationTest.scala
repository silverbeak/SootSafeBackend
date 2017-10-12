package com.sootsafe.engine

import com.sootsafe.engine.StepCalculation.calculateResistanceFromNodeToNextJunction
import com.sootsafe.model.LinkedNode
import com.sootsafe.valuetable.ValueResolver
import org.scalatest.{Matchers, WordSpecLike}

class StepCalculationTest extends WordSpecLike with Matchers with TestFixture {

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
      while (junction.nonEmpty) {
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

      fireFlow should be(114.61397661875115)
    }

    "indicate no added flow from first junction after fire cell" in {
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode
      val fireFlow = StepCalculation.calculateFlowAtPressureDifference(firstJunction, 0, 24.4, 17)

      fireFlow should be(0)
    }

    "indicate no pressure difference between fire cell and first junction" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode.get

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val result = StepCalculation.calculateAggregatedPressure(fireNode, pressureLossTable, 114.61397661875115, 34)

      result should be(0d) // No pressure difference in first junction
    }

    "calculate pressure on fire from first junction" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode().get
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction.get, outletNode)

      val result = StepCalculation.calculateAggregatedPressure(firstJunction, pressureLossTable, 114.61397661875115, 34)

      result should be(27.275027803290786)
    }

    "traverse a model and determine flow during fire" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()

      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction.get, outletNode.get)

      var firePressure_delta_p: Double = 1000
      var aggregatedFireFlow_Q: Double = 0
      var junction: Option[LinkedNode] = fireNode
      var aggregatedRegularPressure_p: Double = 22

      // First calculation, where there is no difference between fire cell and next junction
      aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(junction, firePressure_delta_p, aggregatedRegularPressure_p)

      //aggregatedRegularFlow_q += regularFlow_q
      val regularFlowFromNextJunction_p = StepCalculation.calculateFlowFromNodeToNextJunction(junction)
      firePressure_delta_p = StepCalculation.calculateAggregatedPressure(junction, pressureLossTable, aggregatedFireFlow_Q, regularFlowFromNextJunction_p)

      junction = junction.get.findNextJunction().thisNode

      var regularFlow_q: Double = regularFlowFromNextJunction_p
      // Traverse to the box (the node just before the fan/outlet)
      while (junction.nonEmpty && junction != outletNode) {
        aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(junction, firePressure_delta_p, aggregatedRegularPressure_p, regularFlow_q)

        val regularFlowFromNextJunction_p = StepCalculation.calculateFlowFromNodeToNextJunction(junction)
        firePressure_delta_p += StepCalculation.calculateAggregatedPressure(junction, pressureLossTable, aggregatedFireFlow_Q, regularFlowFromNextJunction_p)

        regularFlow_q = StepCalculation.calculateFlowFromNodeToNextJunction(junction)

        aggregatedRegularPressure_p += calculateResistanceFromNodeToNextJunction(junction, pressureLossTable)

        junction = junction.get.findNextJunction().thisNode
      }

      firePressure_delta_p should be(241.18325024548193)
    }
  }
}
