package com.sootsafe.engine

import com.sootsafe.arithmetic.{Expression, Value}
import com.sootsafe.engine.StepCalculation.calculateResistanceFromNodeToNextJunction
import com.sootsafe.model.{Dimension, LinkedNode, Pipe, PressureLossEntry, SootSafeInfo, TPipe}
import com.sootsafe.valuetable.{FakeValueResolver, ValueResolver}
import org.scalatest.{Matchers, WordSpecLike}

class StepCalculationTest extends WordSpecLike with Matchers with TestFixture {

  val valueResolver: ValueResolver = FakeValueResolver

  "Step Calculation" must {

    "properly calculate the resistance from target node to the next junction" in {
      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode.get
      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val secondJunction = firstJunction.findNextJunction().thisNode

      val pressure = StepCalculation.calculateResistanceFromNodeToNextJunction(secondJunction, pressureLossTable)

      pressure should be(3.3357317620684928)
    }

    "properly calculate the resistance from first junction to the next" in {
      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode().get
      val firstJunction = fireNode.findNextJunction().thisNode.get

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val pressure = StepCalculation.calculateResistanceFromNodeToNextJunction(firstJunction.parent, pressureLossTable)

      pressure should be(2.8264359797675525)
//      pressure should be(2.400202446689589)
    }

    "properly traverse to calculate resistance through a chain" in {
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

      pressure should be(55.544371375939654)
//      pressure should be(54.64488178026985)
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
      val outletNode = linkedModel.locateOutletNode()
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode.get

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)

      val result = StepCalculation.calculateAggregatedPressure(fireNode.get, pressureLossTable, Value(114.61397661875115), Value(34))

      result.calculate() should be(0d) // No pressure difference in first junction
    }

    "calculate pressure on fire from first junction" in {
      val outletNode = linkedModel.locateOutletNode().get
      val fireNode = linkedModel.locateTargetNode()
      val firstJunction = fireNode.get.findNextJunction().thisNode

      val pressureLossTable = new PressureLoss(valueResolver).calculatePressureLoss(firstJunction.get, outletNode)

      val result = StepCalculation.calculateAggregatedPressure(firstJunction.get, pressureLossTable, Value(114.61397661875115), Value(34))

      result.calculate() should be(32.118590679176734)
//      result.calculate() should be(27.275027803290786)
    }

    "traverse a model and determine flow during fire" in {
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

      firePressure_delta_p.toValue should be(Value(259.6815378555465))
//      firePressure_delta_p.toValue should be(Value(243.4302821701289))
    }
  }

  "Calculate pressure difference" must {

    val pressureLossTable = Seq(
      PressureLossEntry(0, 0.0),
      PressureLossEntry(1, 1.1),
      PressureLossEntry(2, 2.2),
      PressureLossEntry(3, 3.3),
      PressureLossEntry(4, 4.4),
    )

    // We don't use a child resolver here, so never mind
    val childResolver = (_: Option[LinkedNode]) => ???

    "return proper value for valid model" in {
      // Define a few test nodes
      val firstNode = LinkedNode(childResolver, Pipe(0, SootSafeInfo("One", None, None, None, None, Dimension(None, None))), None)
      val secondNode = LinkedNode(childResolver, Pipe(1, SootSafeInfo("Two", None, None, None, None, Dimension(None, None))), Some(firstNode))
      val thirdNode = LinkedNode(childResolver, TPipe(2, SootSafeInfo("Three", None, None, None, None, Dimension(None, None))), Some(secondNode))
      val fourthNode = LinkedNode(childResolver, Pipe(3, SootSafeInfo("Four", None, None, None, None, Dimension(None, None))), Some(thirdNode))
      val fifthNode = LinkedNode(childResolver, Pipe(4, SootSafeInfo("Five", None, None, None, None, Dimension(None, None))), Some(fourthNode))

      // The resulting resistance should be 4.4 + 3.3 = 7.7
      // We start from the fifth node, add the value of the fourth node, and then we hit a junction, ending the calculation
      val result = StepCalculation.calculateResistanceFromNodeToNextJunction(Some(fifthNode), pressureLossTable)
      result should be(7.7)
    }

    "return proper value for valid model, also when given node is a junction" in {
      // Define a few test nodes
      val firstNode = LinkedNode(childResolver, Pipe(0, SootSafeInfo("One", None, None, None, None, Dimension(None, None))), None)
      val secondNode = LinkedNode(childResolver, Pipe(1, SootSafeInfo("Two", None, None, None, None, Dimension(None, None))), Some(firstNode))
      val thirdNode = LinkedNode(childResolver, TPipe(2, SootSafeInfo("Three", None, None, None, None, Dimension(None, None))), Some(secondNode))
      val fourthNode = LinkedNode(childResolver, Pipe(3, SootSafeInfo("Four", None, None, None, None, Dimension(None, None))), Some(thirdNode))
      val fifthNode = LinkedNode(childResolver, TPipe(4, SootSafeInfo("Five", None, None, None, None, Dimension(None, None))), Some(fourthNode))

      // The resulting resistance should be 4.4 + 3.3 = 7.7
      // We start from the fifth node, add the value of the fourth node, and then we hit a junction, ending the calculation
      val result = StepCalculation.calculateResistanceFromNodeToNextJunction(Some(fifthNode), pressureLossTable)
      result should be(7.7)
    }
  }
}
