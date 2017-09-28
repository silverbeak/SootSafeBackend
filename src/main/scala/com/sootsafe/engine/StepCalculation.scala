package com.sootsafe.engine

import com.sootsafe.model.{LinkedNode, PressureLossEntry}

object StepCalculation {

  private def findPressureEntry(nodeKey: Int, pressureLossTable: Seq[PressureLossEntry]): Double = {
    pressureLossTable.find(_.id == nodeKey) match {
      case Some(x) => x.pressureLoss
      case None => 0d
    }
  }

  def calculateFlowFromNodeToNextJunction(startNode: Option[LinkedNode], pressureLossTable: Seq[PressureLossEntry]): Double = startNode match {
    case Some(x) =>
      // A "trick" if the startNode is a junction itself
      calculateFlowFromNodeToNextJunction(x.parent, pressureLossTable, findPressureEntry(x.nodeModule.key, pressureLossTable))
    case None => 0d
  }

  private def calculateFlowFromNodeToNextJunction(startNode: Option[LinkedNode], pressureLossTable: Seq[PressureLossEntry], aggregator: Double = 0d): Double = {
    startNode match {
      case Some(n) =>
        if (n.nodeModule.isJunction) aggregator
        else calculateFlowFromNodeToNextJunction(n.parent, pressureLossTable, aggregator + findPressureEntry(n.nodeModule.key, pressureLossTable))
      case None =>
        aggregator
    }
  }
}
