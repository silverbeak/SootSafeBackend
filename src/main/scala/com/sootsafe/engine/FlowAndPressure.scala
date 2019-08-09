package com.sootsafe.engine

import com.sootsafe.arithmetic.{Expression, Value}
import com.sootsafe.model.LinkedNode
import com.sootsafe.server.calculator.SootSafeCalculator.FirePressureResultEntry

object FlowAndPressureSequence {

  def aggregateFlow(seq: Seq[FlowAndPressure]): Value = Value(seq.map(_.addedFireFlow.calculate()).sum)

  def aggregatePressure(seq: Seq[FlowAndPressure]): Double = seq.map(_.firePressureDifference.calculate()).sum

  def toEntries(seq: Seq[FlowAndPressure]): Seq[FirePressureResultEntry] = seq.map(flowAndPressure => {
    new FirePressureResultEntry(
      addedFireFlow = flowAndPressure.addedFireFlow.calculate(),
      firePressureDifference = flowAndPressure.firePressureDifference.calculate(),
      pointRegularPressure = flowAndPressure.pointRegularPressure.calculate(),
      aggregatedRegularFlow = flowAndPressure.aggregatedRegularFlow.calculate(),
      addedRegularFlow = flowAndPressure.addedRegularFlow.calculate(),
      aggregatedFireFlow = flowAndPressure.aggregatedFireFlow.calculate(),
      pointFirePressure = flowAndPressure.pointFirePressure.calculate(),
      regularPressureDifference = flowAndPressure.regularPressureDifference.calculate(),
      key = flowAndPressure.junction.nodeModule.key
    )
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

case class FlowAndPressureEntry(regularFlow: Double,
                                aggregatedRegularFlow: Double,
                                regularPressure: Double,
                                aggregatedRegularPressure: Double)
