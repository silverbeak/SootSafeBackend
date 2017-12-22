package com.sootsafe.engine

import com.sootsafe.arithmetic.{Expression, Value}
import com.sootsafe.model.LinkedNode
import com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.FirePressureResultEntry

object FlowAndPressureSequence {

  def aggregateFlow(seq: Seq[FlowAndPressure]): Value = Value(seq.map(_.addedFireFlow.calculate()).sum)

  def aggregatePressure(seq: Seq[FlowAndPressure]): Double = seq.map(_.firePressureDifference.calculate()).sum

  def toEntries(seq: Seq[FlowAndPressure]): Seq[FirePressureResultEntry] = seq.map(flowAndPressure => {
    val result = FirePressureResultEntry.newBuilder()
    result.setAddedFireFlow(flowAndPressure.addedFireFlow.calculate())
    result.setFirePressureDifference(flowAndPressure.firePressureDifference.calculate())
    result.setPointRegularPressure(flowAndPressure.pointRegularPressure.calculate())
    result.setAggregatedRegularFlow(flowAndPressure.aggregatedRegularFlow.calculate())
    result.setAddedRegularFlow(flowAndPressure.addedRegularFlow.calculate())
    result.setAggregatedFireFlow(flowAndPressure.aggregatedFireFlow.calculate())
    result.setPointFirePressure(flowAndPressure.pointFirePressure.calculate())
    result.setRegularPressureDifference(flowAndPressure.regularPressureDifference.calculate())
    result.setKey(flowAndPressure.junction.nodeModule.key)
    result.build()
  })
}

case class FlowAndPressure(junction: LinkedNode,
                           addedFireFlow: Expression,               // Flödestillskott (brand)
                           firePressureDifference: Expression,      // Tryckskillnad (brand)
                           pointRegularPressure: Expression,        // Tryck (normal)
                           aggregatedRegularFlow: Expression,       // Flöde (normal)
                           addedRegularFlow: Expression,            // Flödestillskott (normal)
                           aggregatedFireFlow: Expression,          // Flöde (brand)
                           pointFirePressure: Expression,           // Tryck (brand)
                           regularPressureDifference: Expression)   // Tryckskillnad (normal)
