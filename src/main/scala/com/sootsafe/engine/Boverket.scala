package com.sootsafe.engine

import com.sootsafe.arithmetic.{Expression, Value}
import com.sootsafe.model.LinkedNode
import com.sootsafe.valuetable.ValueResolver

object Boverket extends PressureLossEngine {

  /**
    *
    * @param linkedModel         The linked model to perform the pressure loss calculation for
    * @param initialFirePressure The initial fire pressure from the fire cell
    * @return
    */
  def calculatePressureLoss(linkedModel: LinkedNode,
                            initialFirePressure: Double,
                            valueResolver: ValueResolver): Either[Seq[FlowAndPressure], String] = {

    linkedModel.locateTargetNode() match {
      case None =>
        Right("Model must contain at least one fire cell")
      case Some(fireNode) =>
        val pressureLossTable = FlowAndPressureHelper.generateRegularPressureLossTable(fireNode, valueResolver)
        val aggregatedRegularPressureLossTable = FlowAndPressureHelper.generateAggregatedRegularPressureLossTable(fireNode)
        val junctionToJunctionPressureLossTable = FlowAndPressureHelper.generateJunctionToJunctionPressureLossTable(fireNode)

        val initialFlowAndPressure = initialStepCalculation(initialFirePressure, fireNode, pressureLossTable, aggregatedRegularPressureLossTable)

        val junctionList = linkedModel.iterateJunctions().toList

        val isOutlet: LinkedNode => Boolean = _.nodeModule.ssInfo.nodeType == "outlet"

        val result = junctionList.foldLeft(Seq(initialFlowAndPressure)) {
          case (aggregator, junction) if !isOutlet(junction) =>

            val pointRegularPressure = aggregator.last.pointRegularPressure - aggregator.last.regularPressureDifference
            val pointFirePressure = aggregator.last.pointFirePressure - aggregator.last.firePressureDifference

            val regularFlowFromNextJunction_q = Value(junction.nodeModule.ssInfo.capacity.getOrElse(0))

            val addedRegularFlow = regularFlowFromNextJunction_q - aggregator.last.aggregatedRegularFlow

            val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(
              pointFirePressure,
              pointRegularPressure,
              addedRegularFlow
            )

            val aggregatedFireFlow_Q = FlowAndPressureSequence.aggregateFlow(aggregator) + addedFireFlow_Q

            val firePressure_delta_p = StepCalculation.calculateAggregatedPressure(
              junction,
              pressureLossTable,
              aggregatedFireFlow_Q,
              regularFlowFromNextJunction_q
            )

            val regularFlowFromThisJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))

            val regularPressureDifference = Value(junctionToJunctionPressureLossTable.getOrElse(junction.nodeModule.key, 0))

            aggregator :+ FlowAndPressure(
              junction,
              addedFireFlow_Q,
              firePressure_delta_p,
              pointRegularPressure,
              regularFlowFromThisJunction_q,
              addedRegularFlow,
              aggregatedFireFlow_Q,
              pointFirePressure,
              regularPressureDifference)

          case (aggregator, junction) =>
            val pointRegularPressure = aggregator.last.pointRegularPressure - aggregator.last.regularPressureDifference
            val pointFirePressure = aggregator.last.pointFirePressure - aggregator.last.firePressureDifference

            aggregator :+ FlowAndPressure(
              junction,
              Expression.NaN,
              Expression.NaN,
              pointRegularPressure,
              Expression.NaN,
              Expression.NaN,
              Expression.NaN,
              pointFirePressure,
              Expression.NaN)
        }

        Left(result)
    }
  }

  /**
    * This is for calculating the first step (where the junction is the fire cell itself)
    * Mostly turning this into a function to isolate fields
    *
    * @param initialFirePressure                The initial fire pressure from the fire cell
    * @param fireNode                           The node to start the calculation from
    * @param pressureLossTable                  The table that holds the pressure loss values
    * @param aggregatedRegularPressureLossTable Table that holds aggregated regular pressure loss
    * @return
    */
  private def initialStepCalculation(initialFirePressure: Double,
                                     fireNode: LinkedNode,
                                     pressureLossTable: Map[Int, Double],
                                     aggregatedRegularPressureLossTable: Map[Int, Double]): FlowAndPressure = {
    ////// (Steg 6) First calculation, where there is no pressure difference between fire cell and next junction

    val initial_firePressure_delta_p = Value(initialFirePressure)

    val regularAggregatedResistance = aggregatedRegularPressureLossTable(fireNode.nodeModule.key)

    val regularFlowFromNextJunction_q = Value(fireNode.nodeModule.ssInfo.capacity.get)

    val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(
      initial_firePressure_delta_p,
      Value(regularAggregatedResistance),
      regularFlowFromNextJunction_q
    )
    ////// (Steg 6) Fire cell calculation done

    ////// (Steg 7) Calculate pressure loss in common duct


    ////// (Steg 7) Calculate pressure loss in common duct done

    val aggregatedFireFlow_Q = addedFireFlow_Q
    val regularFlowFromThisJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(fireNode))

    FlowAndPressure(
      fireNode,
      addedFireFlow_Q,
      Value(initialFirePressure),
      Expression.Zero,
      regularFlowFromThisJunction_q,
      regularFlowFromNextJunction_q,
      aggregatedFireFlow_Q,
      Value(initialFirePressure),
      Value(regularAggregatedResistance)
    )
  }
}
