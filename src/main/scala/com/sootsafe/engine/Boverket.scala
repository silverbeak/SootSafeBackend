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

  private def aggregatedRegularPressureList(linkedNode: LinkedNode, initialPressure: Double, pressureLossTable: Seq[PressureLossEntry]): Seq[Double] = {
    linkedNode.iterateJunctions().foldLeft(Seq[Double](initialPressure)) {
      case (agg, junction) =>
        agg :+ agg.last + calculateResistanceFromNodeToNextJunction(Some(junction), pressureLossTable)
    }
  }

  def calculatePressureLoss(
                             linkedModel: LinkedNode,
                             initialRegularPressure: Double,
                             initialFirePressure: Double = 1000): Double = {

    val fireNode = linkedModel.locateTargetNode()
    val junction = fireNode.get
    val pressureLossTable = getPressureLossTable(linkedModel)

    val aggregatedRegularPressures = aggregatedRegularPressureList(linkedModel, initialRegularPressure, pressureLossTable)

    var firePressure_delta_p: Double = initialFirePressure
    var aggregatedFireFlow_Q: Double = 0
    val aggregatedRegularPressure_p: Double = initialRegularPressure

    var regularFlowFromNextJunction_q: Double = 0

    // First calculation, where there is no difference between fire cell and next junction
    aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(junction, firePressure_delta_p, aggregatedRegularPressure_p, regularFlowFromNextJunction_q).calculate()

    regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))
    firePressure_delta_p = StepCalculation.calculateAggregatedPressure(junction, pressureLossTable, aggregatedFireFlow_Q, regularFlowFromNextJunction_q).calculate()

    // Traverse to the box (the node just before the fan/outlet)
    for {
      (junction, aggregatedRegularPressure_p) <- linkedModel.iterateJunctions().zip(aggregatedRegularPressures.iterator)
    } {
      aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(junction, firePressure_delta_p, aggregatedRegularPressure_p, regularFlowFromNextJunction_q).calculate()

      regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))
      firePressure_delta_p += StepCalculation.calculateAggregatedPressure(junction, pressureLossTable, aggregatedFireFlow_Q, regularFlowFromNextJunction_q).calculate()
    }
    firePressure_delta_p
  }
}
