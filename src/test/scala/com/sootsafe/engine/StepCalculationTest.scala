package com.sootsafe.engine

import com.sootsafe.{Expression, Value}
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

      flow.calculate() should be(17.0)
    }

    "calculate flow at pressure difference" in {
      val fireNode = linkedModel.locateTargetNode()
      val fireFlow = StepCalculation.calculateFlowAtPressureDifference(fireNode.get, Value(1000), Value(22))

      fireFlow.calculate() should be(114.61397661875115)
    }

    "indicate no added flow from first junction after fire cell" in {
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode
      val fireFlow = StepCalculation.calculateFlowAtPressureDifference(firstJunction.get, Value(0), Value(24.4), Value(17))

      fireFlow.calculate() should be(0)
    }

    "indicate no pressure difference between fire cell and first junction" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode.get

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val result = StepCalculation.calculateAggregatedPressure(fireNode.get, pressureLossTable, Value(114.61397661875115), Value(34))

      result.calculate() should be(0d) // No pressure difference in first junction
    }

    "calculate pressure on fire from first junction" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode().get
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction.get, outletNode)

      val result = StepCalculation.calculateAggregatedPressure(firstJunction.get, pressureLossTable, Value(114.61397661875115), Value(34))

      result.calculate() should be(27.275027803290786)
    }

    "traverse a model and determine flow during fire" in {
      val valueResolver: ValueResolver = new ValueResolver {}

      val outletNode = linkedModel.locateOutletNode()

      val fireNode = linkedModel.locateTargetNode()

      val junctionIterator = linkedModel.iterateJunctions()
      val firstJunction = junctionIterator.next()

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      var firePressure_delta_p: Expression = Value(1000)
      var aggregatedFireFlow_Q: Expression = Expression.Zero
      var aggregatedRegularPressure_p: Double = 22

      // First calculation, where there is no difference between fire cell and next junction
      aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(fireNode.get, firePressure_delta_p, Value(aggregatedRegularPressure_p))

      //aggregatedRegularFlow_q += regularFlow_q
      val regularFlowFromNextJunction_p = StepCalculation.calculateFlowFromNodeToNextJunction(fireNode)
      firePressure_delta_p = StepCalculation.calculateAggregatedPressure(fireNode.get, pressureLossTable, aggregatedFireFlow_Q.toValue, regularFlowFromNextJunction_p.toValue)

      var regularFlow_q = regularFlowFromNextJunction_p
      // Traverse to the box (the node just before the fan/outlet)
      for {
        junction <- linkedModel.iterateJunctions()
      } {
        aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(junction, firePressure_delta_p, Value(aggregatedRegularPressure_p), regularFlow_q.toValue)

        val regularFlowFromNextJunction_p = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))
        firePressure_delta_p += StepCalculation.calculateAggregatedPressure(junction, pressureLossTable, aggregatedFireFlow_Q.toValue, regularFlowFromNextJunction_p.toValue)

        regularFlow_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))

        aggregatedRegularPressure_p += calculateResistanceFromNodeToNextJunction(Some(junction), pressureLossTable)
      }

      firePressure_delta_p.toValue should be(Value(241.18325024548193))
    }
  }
}
