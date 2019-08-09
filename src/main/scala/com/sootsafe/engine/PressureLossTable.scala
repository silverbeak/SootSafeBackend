package com.sootsafe.engine

import com.sootsafe.model.LinkedNode
import com.sootsafe.valuetable.ValueResolver

object PressureLossTable {

  def mergeTables(pointRegularPressureLossTable: Map[Int, Double],
                  aggregatedRegularPressureLossTable: Map[Int, Double],
                  regularFlowTable: Map[Int, Double]): Map[Int, PressureLossRow] = {
    // convert maps to seq, to keep duplicate keys and concat
    val merged = pointRegularPressureLossTable.toSeq ++ aggregatedRegularPressureLossTable.toSeq ++ regularFlowTable.toSeq

    merged
      .groupBy(_._1)
      .map {
        case (key, values) => key -> PressureLossRow(key, values.head._2, values(1)._2, values(2)._2)
      }
  }

  def createAllTables(fireNode: LinkedNode, valueResolver: ValueResolver): Map[Int, PressureLossRow] = {
    val pressureLossTable = FlowAndPressureHelper.generateRegularPressureLossTable(fireNode, valueResolver)
    val aggregatedRegularPressureLossTable = FlowAndPressureHelper.generateAggregatedRegularPressureLossTable(fireNode)
    val regularFlowTable = FlowAndPressureHelper.generateRegularFlowTable(fireNode)

    mergeTables(pressureLossTable, aggregatedRegularPressureLossTable, regularFlowTable)
  }

}

case class PressureLossRow(key: Int,
                           pointRegularPressureLoss: Double,
                           aggregatedRegularPressureLoss: Double,
                           regularFlow: Double)
