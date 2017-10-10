package com.sootsafe.engine

import com.sootsafe.engine.StepCalculation.calculateResistanceFromNodeToNextJunction
import com.sootsafe.model.LinkedNode
import com.sootsafe.valuetable.ValueResolver

trait PressureLossEngine {
  def calculatePressureLoss(linkedModel: LinkedNode): Double
}

object Boverket extends PressureLossEngine {
  def calculatePressureLoss(linkedModel: LinkedNode): Double = {
    val valueResolver = new ValueResolver {}
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
    firePressure_delta_p
  }
}
