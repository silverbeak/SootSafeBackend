package com.sootsafe.engine

import com.sootsafe.Expression
import com.sootsafe.model.LinkedNode
import com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.FirePressureResultEntry

object FlowAndPressureSequence {

  def aggregateFlow(seq: Seq[FlowAndPressure]): Double = seq.map(_.flow.calculate()).sum

  def aggregatePressure(seq: Seq[FlowAndPressure]): Double = seq.map(_.pressure.calculate()).sum

  def toEntries(seq: Seq[FlowAndPressure]): Seq[FirePressureResultEntry] = seq.map(flowAndPressure => {
    val result = FirePressureResultEntry.newBuilder()
    result.setFlow(flowAndPressure.flow.calculate())
    result.setPressure(flowAndPressure.pressure.calculate())
    result.setKey(flowAndPressure.junction.nodeModule.key)
    result.build()
  })
}

case class FlowAndPressure(junction: LinkedNode, flow: Expression, pressure: Expression)

