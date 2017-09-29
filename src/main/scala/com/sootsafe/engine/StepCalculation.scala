package com.sootsafe.engine

import com.sootsafe.model.{LinkedNode, PressureLossEntry}

object StepCalculation {

  private def findPressureEntry(nodeKey: Int, pressureLossTable: Seq[PressureLossEntry]): Double = {
    pressureLossTable.find(_.id == nodeKey) match {
      case Some(x) => x.pressureLoss
      case None => 0d
    }
  }

  def calculateResistanceFromNodeToNextJunction(startNode: Option[LinkedNode], pressureLossTable: Seq[PressureLossEntry]): Double = startNode match {
    case Some(x) =>
      // A "trick" if the startNode is a junction itself
      calculateResistanceFromNodeToNextJunction(x.parent, pressureLossTable, findPressureEntry(x.nodeModule.key, pressureLossTable))
    case None => 0d
  }

  private def calculateResistanceFromNodeToNextJunction(startNode: Option[LinkedNode], pressureLossTable: Seq[PressureLossEntry], aggregator: Double = 0d): Double = {
    startNode match {
      case Some(n) if n.nodeModule.isJunction => aggregator
      case Some(n) => calculateResistanceFromNodeToNextJunction(n.parent, pressureLossTable, aggregator + findPressureEntry(n.nodeModule.key, pressureLossTable))
      case None => aggregator
    }
  }

  def calculateFlowFromNodeToNextJunction(startNode: Option[LinkedNode]): Double = startNode match {
    case Some(x) => calculateFlowFromNodeToNextJunction(x.parent, x.nodeModule.ssInfo.capacity.getOrElse(0d))
    case None => 0d
  }

  private def calculateFlowFromNodeToNextJunction(startNode: Option[LinkedNode], max: Double = 0d): Double = startNode match {
    case Some(x) if x.nodeModule.isJunction => Math.max(max, x.nodeModule.ssInfo.capacity.getOrElse(0d))
    case Some(x) => calculateFlowFromNodeToNextJunction(x.parent, Math.max(max, x.nodeModule.ssInfo.capacity.getOrElse(0d)))
    case None => max
  }

  def calculateFlowAtPressureDifference(startNode: Option[LinkedNode], firePressure: Double, regularPressure: Double): Double = {
    val originalFlow = calculateFlowFromNodeToNextJunction(startNode)
    originalFlow * Math.sqrt(firePressure/regularPressure)
  }
}
