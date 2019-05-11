package com.sootsafe.engine

import com.sootsafe.arithmetic.{Expression, Value}
import com.sootsafe.model.{LinkedNode, NodeIterator}
import com.sootsafe.valuetable.ValueResolver

trait PressureLossEngine {
  def calculatePressureLoss(linkedModel: LinkedNode,
                            initialRegularPressure: Option[Double] = None,
                            initialFirePressure: Double,
                            valueResolver: ValueResolver): Either[Seq[FlowAndPressure], String]
}

object Boverket extends PressureLossEngine {

  /**
    *
    * @param linkedModel                  The linked model to perform the pressure loss calculation for
    * @param initialRegularPressureOption If set, this will override the initial regular pressure found in the fire cell in the model
    * @param initialFirePressure          The initial fire pressure from the fire cell
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

        val pressureLossTable = FlowAndPressureHelper.generateRegularPressureLossTable(fireNode, valueResolver)
        val aggregatedRegularResistanceTable = FlowAndPressureHelper.generateAggregatedRegularPressureTable(fireNode)
        val regularFlowTable = FlowAndPressureHelper.generateRegularFlowTable(fireNode)


        ////// First calculation, where there is no pressure difference between fire cell and next junction

        val nextJunction = fireNode.findNextJunction()
        val initial_firePressure_delta_p = Value(initialFirePressure)

        val regularAggregatedResistance = aggregatedRegularResistanceTable(nextJunction.previousNode.get.nodeModule.key)

        val regularFlowFromNextJunction_q = Value(regularFlowTable(nextJunction.thisNode.get.nodeModule.key))

        val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(
          fireNode,
          initial_firePressure_delta_p,
          Value(regularAggregatedResistance),
          regularFlowFromNextJunction_q
        )

        val aggregatedRegularFlow = StepCalculation.flowAtNextJunction(fireNode)
        val aggregatedFireFlow_Q = /*FlowAndPressureSequence.aggregateFlow(aggregator) +*/ addedFireFlow_Q

        val firePressure_delta_p = StepCalculation.calculateAggregatedPressure(
          nextJunction.thisNode.get,
          pressureLossTable,
          addedFireFlow_Q.toValue,
          aggregatedRegularFlow
        )

        val regularFlowFromThisJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(fireNode))

        val initialFlowAndPressure = FlowAndPressure(
          fireNode,
          addedFireFlow_Q,
          firePressure_delta_p,
          Value(aggregatedRegularResistanceTable(fireNode.nodeModule.key)),
          regularFlowFromThisJunction_q,
          regularFlowFromNextJunction_q,
          aggregatedFireFlow_Q,
          Value(initialFirePressure),
          Value(initialRegularPressure)
        )
        ////// Fire cell calculation done

        // Note, this can be done without the 'toList' part. Use as iterators instead
        val junctionList = linkedModel.iterateJunctions().toList

        val result = junctionList.foldLeft(Seq(initialFlowAndPressure)) {
          case (aggregator, junction) if junction.findNextJunction().thisNode.isDefined =>

            val nextJunction = junction.findNextJunction()
            val aggregatedFirePressure_delta_p = Value(FlowAndPressureSequence.aggregatePressure(aggregator))

            val regularAggregatedResistance = aggregatedRegularResistanceTable(nextJunction.previousNode.get.nodeModule.key)

            val regularFlowFromNextJunction_q = Value(regularFlowTable(nextJunction.thisNode.get.nodeModule.key))

            val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(
              junction,
              aggregatedFirePressure_delta_p,
              Value(regularAggregatedResistance),
              regularFlowFromNextJunction_q
            )

            val aggregatedRegularFlow = StepCalculation.flowAtNextJunction(junction)
            val aggregatedFireFlow_Q = FlowAndPressureSequence.aggregateFlow(aggregator) + addedFireFlow_Q

            val firePressure_delta_p = StepCalculation.calculateAggregatedPressure(
              nextJunction.thisNode.get,
              pressureLossTable,
              aggregatedFireFlow_Q,
              aggregatedRegularFlow
            )

            val regularFlowFromThisJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))

            aggregator :+ FlowAndPressure(
              junction,
              addedFireFlow_Q,
              firePressure_delta_p,
              Value(aggregatedRegularResistanceTable(junction.nodeModule.key)),
              regularFlowFromThisJunction_q,
              regularFlowFromNextJunction_q,
              aggregatedFireFlow_Q,
              aggregatedFirePressure_delta_p,
              Expression.Zero) /* TODO: This one's not right */

          case (aggregator, _) => aggregator
        }

        Left(result)
    }
  }
}

case class FlowAndPressureEntry(regularFlow: Double,
                                aggregatedRegularFlow: Double,
                                regularPressure: Double,
                                aggregatedRegularPressure: Double)

object FlowAndPressureHelper {

  def generateRegularPressureLossTable(linkedModel: LinkedNode, valueResolver: ValueResolver): Map[Int, Double] = {
    new NodeIterator(Option(linkedModel))
      .map(node => node.nodeModule.key -> node.nodeModule.ssInfo.pressureloss.getOrElse(0d))
      .toMap
  }

  def generateAggregatedRegularPressureTable(linkedNode: LinkedNode): Map[Int, Double] = {
    NodeIterator(linkedNode)
      .foldLeft((Map[Int, Double](), 0d)) {
        case (aggregator, node) =>
          val pressureValue = node.nodeModule.ssInfo.pressureloss.get
          (
            aggregator._1 + (node.nodeModule.key -> (aggregator._2 + pressureValue)),
            aggregator._2 + pressureValue
          )
      }._1
  }

  def generateRegularFlowTable(linkedNode: LinkedNode): Map[Int, Double] = {
    // Not pretty. But we only want to calculate flow for cells, junctions and boxes
    val allowedNodeTypes = Seq("fireCell", "tpipe", "box")

    NodeIterator(linkedNode)
      .filter(n => allowedNodeTypes.contains(n.nodeModule.ssInfo.nodeType))
      .foldLeft((Map[Int, Double](), 0d)) {
        case (aggregator, node) =>
          val flowValue = node.nodeModule.ssInfo.capacity.getOrElse(0d)
          (
            aggregator._1 + (node.nodeModule.key -> (flowValue - aggregator._2)),
            flowValue
          )
      }._1
  }
}