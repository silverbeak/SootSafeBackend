package com.sootsafe.engine

import com.sootsafe.arithmetic.{Expression, Value}
import com.sootsafe.engine.StepCalculation.calculateResistanceFromNodeToNextJunction
import com.sootsafe.model.{LinkedNode, NodeIterator, PressureLossEntry}
import com.sootsafe.valuetable.ValueResolver

trait PressureLossEngine {
  def calculatePressureLoss(linkedModel: LinkedNode,
                            initialRegularPressure: Option[Double] = None,
                            initialFirePressure: Double,
                            valueResolver: ValueResolver): Either[Seq[FlowAndPressure], String]
}

object Boverket extends PressureLossEngine {

  private def getPressureLossTable(linkedModel: LinkedNode, valueResolver: ValueResolver): Seq[PressureLossEntry] = {
    new PressureLoss(valueResolver).calculatePressureLoss2(linkedModel)
  }

  private def aggregatedRegularPressureList2(linkedNode: LinkedNode,
                                             pressureLossTable: Seq[PressureLossEntry]): Seq[PressureLossEntry] = {
    NodeIterator(linkedNode)
      .foldLeft(Seq[PressureLossEntry]()) {
        case (aggregator, node) if aggregator.isEmpty =>
          aggregator :+ PressureLossEntry(
            node.nodeModule.key,
            node.nodeModule.ssInfo.pressureloss.get
          )

        case (aggregator, node) =>
          aggregator :+ PressureLossEntry(
            node.nodeModule.key,
            node.nodeModule.ssInfo.pressureloss.get + aggregator.last.pressureLoss
          )
      }
      .toList
  }

  private def aggregatedRegularPressureList(linkedNode: LinkedNode,
                                            initialPressure: Double,
                                            pressureLossTable: Seq[PressureLossEntry]): Seq[Double] = {
    linkedNode.iterateJunctions().foldLeft(Seq[Double](initialPressure)) {
      case (agg, junction) =>
        agg :+ agg.last + calculateResistanceFromNodeToNextJunction(Some(junction), pressureLossTable)
    }
  }

  def generateFlowTable(fireNode: LinkedNode): Map[Int, Double] = {
    NodeIterator(fireNode)
      .map(node => node.nodeModule.key -> node.nodeModule.ssInfo.capacity.getOrElse(0d))
      .toMap
  }

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
        val pressureLossTable = getPressureLossTable(fireNode, valueResolver)
        val aggregatedRegularPressures = aggregatedRegularPressureList(linkedModel, initialRegularPressure, pressureLossTable)

        val aggregatedRegularResistanceList = aggregatedRegularPressureList2(fireNode, pressureLossTable)
        val flowTable = generateFlowTable(fireNode)

        var previousFlow: Expression = Value(flowTable(fireNode.nodeModule.key))

        val initial_firePressure_delta_p = Value(initialFirePressure)
//        val regularAggregatedResistance = Value(calculateResistanceFromNodeToNextJunction(Option(fireNode), pressureLossTable))
        val regularAggregatedResistance = aggregatedRegularResistanceList.find(n => n.id == fireNode.findNextJunction().previousNode.get.nodeModule.key).get.pressureLoss

        val regularFlowFromNextJunction_q: Expression = Value(flowTable(fireNode.findNextJunction().thisNode.get.nodeModule.key)) - previousFlow
        previousFlow += regularFlowFromNextJunction_q.toValue

        ////// First calculation, where there is no difference between fire cell and next junction
        val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(
          fireNode,
          initial_firePressure_delta_p,
          Value(regularAggregatedResistance),
          regularFlowFromNextJunction_q
        )

//        regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(fireNode))

        val aggregatedRegularFlow = StepCalculation.flowAtNextJunction(fireNode)

        val firePressure_delta_p = StepCalculation.calculateAggregatedPressure(
          fireNode.findNextJunction().thisNode.get,
          pressureLossTable,
          addedFireFlow_Q.toValue,
          aggregatedRegularFlow
        )

        val aggregatedFireFlow_Q = addedFireFlow_Q

        val initialFlowAndPressure = FlowAndPressure(
          fireNode,
          addedFireFlow_Q,
          firePressure_delta_p,
          Expression.Zero,
          regularFlowFromNextJunction_q,
          Expression.Zero,
          aggregatedFireFlow_Q,
          Value(initialFirePressure),
          Value(initialRegularPressure)
        )
        ////// Fire cell calculation done

        // Note, this can be done without the 'toList' part. Use as iterators instead
        val junctionList = linkedModel.iterateJunctions().toList
        val aggregatedRegularPressureSeq = aggregatedRegularPressures.iterator.toList.tail

        val modelAndPressureIterator = junctionList zip aggregatedRegularPressureSeq

        // Traverse to the box (the node just before the fan/outlet)
        val result = modelAndPressureIterator.foldLeft(Seq(initialFlowAndPressure)) {
          case (aggregator, (junction, aggregatedRegularPressure_p)) if junction.findNextJunction().thisNode.isDefined =>

            val nextJunction = junction.findNextJunction()
            val aggregatedFirePressure_delta_p = Value(FlowAndPressureSequence.aggregatePressure(aggregator))

            var regularFlowFromNextJunction_q: Expression = Value(flowTable(nextJunction.thisNode.get.nodeModule.key)) - previousFlow.toValue
            previousFlow += regularFlowFromNextJunction_q.toValue

            val addedRegularFlow = regularFlowFromNextJunction_q

            //            val aggregated_firePressure = aggregatedRegularResistanceList.find(_.id == junction.nodeModule.key).get.pressureLoss
            val regularAggregatedResistance = aggregatedRegularResistanceList.find(n => n.id == nextJunction.previousNode.get.nodeModule.key).get.pressureLoss

            val addedFireFlow_Q = StepCalculation.calculateFlowAtPressureDifference(
              junction,
              aggregatedFirePressure_delta_p,
              Value(regularAggregatedResistance),
              regularFlowFromNextJunction_q
            )

            regularFlowFromNextJunction_q = StepCalculation.calculateFlowFromNodeToNextJunction(Some(junction))
            val aggregatedRegularFlow = StepCalculation.flowAtNextJunction(junction)

            val aggregatedFireFlow_Q = FlowAndPressureSequence.aggregateFlow(aggregator) + addedFireFlow_Q
            val firePressure_delta_p = StepCalculation.calculateAggregatedPressure(
              nextJunction.thisNode.get,
              pressureLossTable,
              aggregatedFireFlow_Q,
              aggregatedRegularFlow
            )

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

          case (aggregator, _) => aggregator
        }

        Left(result)
    }
  }
}
