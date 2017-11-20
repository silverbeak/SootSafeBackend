package com.sootsafe.engine

import com.sootsafe.engine.StepCalculation.calculateResistanceFromNodeToNextJunction
import com.sootsafe.model.{LinkedNode, PressureLossEntry}
import com.sootsafe.valuetable.ValueResolver

trait PressureLossEngine {
  def calculatePressureLoss(
                             linkedModel: LinkedNode,
                             initialRegularPressure: Double,
                             initialFirePressure: Double): Double
}

object Boverket extends PressureLossEngine {

  private def getPressureLossTable(linkedModel: LinkedNode): Seq[PressureLossEntry] = {
    val valueResolver = new ValueResolver {}
    val outletNode = linkedModel.locateOutletNode()

    val firstJunction = linkedModel.iterateJunctions().next()
    new PressureLoss(valueResolver).calculatePressureLoss(firstJunction, outletNode.get)
  }

  def calculatePressureLoss(
                             linkedModel: LinkedNode,
                             initialRegularPressure: Double,
                             initialFirePressure: Double = 1000): Double = {

    val fireNode = linkedModel.locateTargetNode()

    val pressureLossTable = getPressureLossTable(linkedModel)

    var firePressure_delta_p: Double = initialFirePressure
    var aggregatedFireFlow_Q: Double = 0
    val junction: Option[LinkedNode] = fireNode
    var aggregatedRegularPressure_p: Double = initialRegularPressure

    // First calculation, where there is no difference between fire cell and next junction
    aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(junction.get, firePressure_delta_p, aggregatedRegularPressure_p).calculate()

    //aggregatedRegularFlow_q += regularFlow_q
    var regularFlowFromNextJunction_q: Double = StepCalculation.calculateFlowFromNodeToNextJunction(junction)
    firePressure_delta_p = StepCalculation.calculateAggregatedPressure(junction.get, pressureLossTable, aggregatedFireFlow_Q, regularFlowFromNextJunction_q).calculate()

    // Traverse to the box (the node just before the fan/outlet)
    for {
      junction <- linkedModel.iterateJunctions()
    } {
      aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(junction, firePressure_delta_p, aggregatedRegularPressure_p, regularFlowFromNextJunction_q).calculate()

      regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))
      firePressure_delta_p += StepCalculation.calculateAggregatedPressure(junction, pressureLossTable, aggregatedFireFlow_Q, regularFlowFromNextJunction_q).calculate()

      aggregatedRegularPressure_p += calculateResistanceFromNodeToNextJunction(Some(junction), pressureLossTable)
    }
    firePressure_delta_p
  }
}
