package com.sootsafe.engine

import com.sootsafe.engine.StepCalculation.calculateResistanceFromNodeToNextJunction
import com.sootsafe.model.{LinkedNode, PressureLossEntry}
import com.sootsafe.valuetable.ValueResolver
import com.sootsafe.{Expression, Value}

trait PressureLossEngine {
  def calculatePressureLoss(linkedModel: LinkedNode,
                            initialRegularPressure: Option[Double],
                            initialFirePressure: Double,
                            valueResolver: ValueResolver): Either[Seq[FlowAndPressure], String]
}

object Boverket extends PressureLossEngine {

  private def getPressureLossTable(linkedModel: LinkedNode, valueResolver: ValueResolver): Seq[PressureLossEntry] = {
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

  /**
    *
    * @param linkedModel The linked model to perform the pressure loss calculation for
    * @param initialRegularPressureOption If set, this will override the initial regular pressure found in the fire cell in the model
    * @param initialFirePressure The initial fire pressure from the fire cell
    * @return
    */
  def calculatePressureLoss(linkedModel: LinkedNode,
                            initialRegularPressureOption: Option[Double] = None,
                            initialFirePressure: Double,
                            valueResolver: ValueResolver): Either[Seq[FlowAndPressure], String] = {

    linkedModel.locateTargetNode() match {
      case None =>
        Right("Model must contain at least one fire cell")
      case Some(fireNode) =>
        val initialRegularPressure = initialRegularPressureOption.getOrElse(fireNode.nodeModule.ssInfo.pressureloss.getOrElse(0d))
        val pressureLossTable = getPressureLossTable(linkedModel, valueResolver)

        val aggregatedRegularPressures = aggregatedRegularPressureList(linkedModel, initialRegularPressure, pressureLossTable)

        var firePressure_delta_p: Expression = Value(initialFirePressure)

        var regularFlowFromNextJunction_q: Expression = Expression.Zero

        ////// First calculation, where there is no difference between fire cell and next junction
        val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(fireNode, firePressure_delta_p, Value(initialRegularPressure), regularFlowFromNextJunction_q)

        regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(fireNode))
        firePressure_delta_p = StepCalculation.calculateAggregatedPressure(fireNode, pressureLossTable, addedFireFlow_Q.toValue, regularFlowFromNextJunction_q)

        val aggregatedFireFlow_Q = addedFireFlow_Q

        val initialFlowAndPressure = FlowAndPressure(
          fireNode, addedFireFlow_Q,
          firePressure_delta_p,
          Expression.Zero,
          regularFlowFromNextJunction_q,
          Expression.Zero,
          aggregatedFireFlow_Q,
          Value(initialFirePressure),
          Value(initialRegularPressure)
        )
        ////// Fire cell calculation done

        val modelAndPressureIterator: Iterator[(LinkedNode, Double)] = linkedModel.iterateJunctions().zip(aggregatedRegularPressures.iterator)

        // Traverse to the box (the node just before the fan/outlet)
        val result = modelAndPressureIterator.foldLeft(Seq(initialFlowAndPressure)) {
          case (aggregator, (junction, aggregatedRegularPressure_p)) =>
            val aggregatedFirePressure_delta_p = Value(FlowAndPressureSequence.aggregatePressure(aggregator))

            val addedRegularFlow = regularFlowFromNextJunction_q

            val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(junction, aggregatedFirePressure_delta_p, Value(aggregatedRegularPressure_p), regularFlowFromNextJunction_q)

            regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))
            val aggregatedFireFlow_Q = FlowAndPressureSequence.aggregateFlow(aggregator) + addedFireFlow_Q
            val firePressure_delta_p = StepCalculation.calculateAggregatedPressure(junction, pressureLossTable, aggregatedFireFlow_Q, regularFlowFromNextJunction_q)

            aggregator :+ FlowAndPressure(
              junction,
              addedFireFlow_Q,
              firePressure_delta_p,
              Value(aggregatedRegularPressure_p),
              regularFlowFromNextJunction_q,
              addedRegularFlow,
              aggregatedFireFlow_Q,
              aggregatedFirePressure_delta_p,
              Expression.Zero) /* TODO: This one's not right */
        }
        Left(result)
    }
  }
}
