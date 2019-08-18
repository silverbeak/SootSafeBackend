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

        val result = junctionList.foldLeft(Seq(initialFlowAndPressure)) {
          case (aggregator, junction) if junction.findNextJunction().previousNode.isDefined =>

            val aggregatedFirePressure_delta_p = FlowAndPressureSequence.aggregatePressure(aggregator)

            val nextJunction = junction.findNextJunction()
            val regularAggregatedResistance = aggregatedRegularPressureLossTable(nextJunction.previousNode.get.nodeModule.key)

            val regularFlowFromNextJunction_q = Value(nextJunction.thisNode.get.nodeModule.ssInfo.capacity.getOrElse(0)) // Value(regularFlowTable(nextJunction.thisNode.get.nodeModule.key))

            val aggregatedRegularFlow = Value(nextJunction.thisNode.get.nodeModule.ssInfo.capacity.getOrElse(0)) // StepCalculation.flowAtNextJunction(junction)
            val aggregatedFireFlow = aggregator.last.aggregatedFireFlow

            val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(
              junction,
              Value(aggregatedFirePressure_delta_p),
              Value(regularAggregatedResistance),
              regularFlowFromNextJunction_q
            )

            //            val aggregatedRegularFlow = StepCalculation.flowAtNextJunction(nextJunction.thisNode.get)

            val firePressure_delta_p = StepCalculation.calculateAggregatedPressure(
              nextJunction.thisNode.get,
              pressureLossTable,
              aggregatedFireFlow + addedFireFlow_Q,
              aggregatedRegularFlow
            )

            val aggregatedFireFlow_Q = FlowAndPressureSequence.aggregateFlow(aggregator)
            val regularFlowFromThisJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))

            aggregator :+ FlowAndPressure(
              junction,
              addedFireFlow_Q,
              firePressure_delta_p,
              Value(aggregatedRegularPressureLossTable(junction.nodeModule.key)),
              regularFlowFromThisJunction_q,
              regularFlowFromNextJunction_q,
              aggregatedFireFlow_Q + addedFireFlow_Q,
              Value(aggregatedFirePressure_delta_p),
              Value(junctionToJunctionPressureLossTable(nextJunction.thisNode.get.nodeModule.key)))

          case (aggregator, junction) =>

            val aggregatedFirePressure_delta_p = FlowAndPressureSequence.aggregatePressure(aggregator)

            aggregator :+ FlowAndPressure(
              junction,
              Expression.Zero,
              Expression.Zero,
              Value(aggregatedRegularPressureLossTable(junction.nodeModule.key)),
              Expression.Zero,
              Expression.Zero,
              Expression.Zero,
              Value(aggregatedFirePressure_delta_p),
              Value(junctionToJunctionPressureLossTable(junction.nodeModule.key)))
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

    val regularFlowFromNextJunction_q = Expression.Zero

    val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(
      fireNode,
      initial_firePressure_delta_p,
      Value(regularAggregatedResistance),
      regularFlowFromNextJunction_q
    )
    ////// (Steg 6) Fire cell calculation done

    ////// (Steg 7) Calculate pressure loss in common duct


    val nextJunction = fireNode.findNextJunction()
    val aggregatedRegularFlow = StepCalculation.flowAtNextJunction(nextJunction.thisNode.get)

    val firePressure_delta_p = StepCalculation.calculateAggregatedPressure(
      nextJunction.thisNode.get,
      pressureLossTable,
      addedFireFlow_Q.toValue,
      aggregatedRegularFlow
    )

    ////// (Steg 7) Calculate pressure loss in common duct done

    val aggregatedFireFlow_Q = /*FlowAndPressureSequence.aggregateFlow(aggregator) +*/ addedFireFlow_Q
    val regularFlowFromThisJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(fireNode))

    FlowAndPressure(
      fireNode,
      addedFireFlow_Q,
      firePressure_delta_p,
      Value(aggregatedRegularPressureLossTable(fireNode.nodeModule.key)),
      regularFlowFromThisJunction_q,
      regularFlowFromNextJunction_q,
      aggregatedFireFlow_Q,
      Value(initialFirePressure),
      Value(regularAggregatedResistance)
    )
  }
}
