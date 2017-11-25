package com.sootsafe.engine

import com.sootsafe.{Expression, Value}
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

    val fireNode = linkedModel.locateTargetNode().get
    val pressureLossTable = getPressureLossTable(linkedModel)

    val aggregatedRegularPressures = aggregatedRegularPressureList(linkedModel, initialRegularPressure, pressureLossTable)

    var firePressure_delta_p: Expression = Value(initialFirePressure)
    var aggregatedFireFlow_Q: Expression = Expression.empty

    var regularFlowFromNextJunction_q: Double = 0

    ////// First calculation, where there is no difference between fire cell and next junction
    aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(fireNode, firePressure_delta_p.calculate(), initialRegularPressure, regularFlowFromNextJunction_q)

    regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(fireNode))
    firePressure_delta_p = StepCalculation.calculateAggregatedPressure(fireNode, pressureLossTable, aggregatedFireFlow_Q.calculate(), regularFlowFromNextJunction_q)

    val flowAndPressureResult = Seq[FlowAndPressure](FlowAndPressure(fireNode, aggregatedFireFlow_Q, firePressure_delta_p))
    ////// Fire cell calculation done

    val modelAndPressureIterator: Iterator[(LinkedNode, Double)] = linkedModel.iterateJunctions().zip(aggregatedRegularPressures.iterator)

    // Traverse to the box (the node just before the fan/outlet)
    val result = modelAndPressureIterator.foldLeft(flowAndPressureResult) {
      case (aggregator, (junction, aggregatedRegularPressure_p)) =>
        aggregatedFireFlow_Q += StepCalculation.calculateFlowAtPressureDifference(junction, firePressure_delta_p.calculate(), aggregatedRegularPressure_p, regularFlowFromNextJunction_q)

        regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))
        firePressure_delta_p += StepCalculation.calculateAggregatedPressure(junction, pressureLossTable, aggregatedFireFlow_Q.calculate(), regularFlowFromNextJunction_q)

        aggregator :+ FlowAndPressure(junction, aggregatedFireFlow_Q, firePressure_delta_p)
    }

    result.last.pressure.calculate()
  }
}

case class FlowAndPressure(junction: LinkedNode, flow: Expression, pressure: Expression)
