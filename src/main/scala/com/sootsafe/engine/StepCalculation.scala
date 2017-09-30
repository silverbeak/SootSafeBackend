package com.sootsafe.engine

import com.sootsafe.model.{LinkedNode, PressureLossEntry}

object StepCalculation {

  /**
    * Looking up the pressure (in Pascal) at a certain point according to the given "table"
    *
    * @param nodeKey           The key for the node we want to find the pressure for
    * @param pressureLossTable The "table" containing node keys as key and the pressure as value
    * @return
    */
  private def findPressureEntry(nodeKey: Int, pressureLossTable: Seq[PressureLossEntry]): Double = {
    pressureLossTable.find(_.id == nodeKey) match {
      case Some(x) => x.pressureLoss
      case None => 0d
    }
  }

  /**
    * Calculate the resistance (in Pascal) between a given node to the next junction.
    * If a start node is not specified, the returned pressure will be 0
    * If no junction is found before the model "ends", the returned value will be the resistance between the given node and the last node in the model
    *
    * @param startNode         The node to start this calculation from
    * @param pressureLossTable The pressure loss table for the system
    * @return The resistance (in Pascal) between a given node to the next junction
    */
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

  /**
    * Returns the maximum flow (from given values) from one node to the next junction.
    * If no start node is defined, the returned flow will be 0
    * If no junction is found before the model "ends", the returned flow will be be between the given node and the last node in the model
    *
    * @param startNode The node to start the calculation from
    * @return The flow (in l/s) between the given node and the next junction
    */
  def calculateFlowFromNodeToNextJunction(startNode: Option[LinkedNode]): Double = startNode match {
    case Some(x) => calculateFlowFromNodeToNextJunction(x.parent, x.nodeModule.ssInfo.capacity.getOrElse(0d))
    case None => 0d
  }

  private def calculateFlowFromNodeToNextJunction(startNode: Option[LinkedNode], max: Double = 0d): Double = startNode match {
    case Some(x) if x.nodeModule.isJunction => max
    case Some(x) => calculateFlowFromNodeToNextJunction(x.parent, Math.max(max, x.nodeModule.ssInfo.capacity.getOrElse(0d)))
    case None => max
  }

  /**
    * Boverket 1994:13 Appendix B, steg 6
    *
    * @param startNode       The node to start this calculation from
    * @param firePressure    The pressure (in Pascal) during fire (boverket schablon: 1000 Pa)
    * @param regularPressure The pressure (in Pascal) during regular circumstances
    * @return The flow (in l/s) through this point
    */
  def calculateFlowAtPressureDifference(startNode: Option[LinkedNode], firePressure: Double, regularPressure: Double): Double = {
    val originalFlow = calculateFlowFromNodeToNextJunction(startNode)
    originalFlow * Math.sqrt(firePressure / regularPressure)
  }

  /**
    * Boverket 1994:13 Appendix B steg 7
    *
    * @param startNode         The node to start this calculation from
    * @param pressureLossTable The pressure "table" for the entire model
    * @param firePressure      The pressure in this point during fire
    * @param regularPressure   The regular pressure in this point
    * @return The pressure delta (in Pascal) in this point between regular circumstances and during a fire
    */
  def calculateDeltaPressureOnFire(startNode: Option[LinkedNode], pressureLossTable: Seq[PressureLossEntry], firePressure: Double, regularPressure: Double): Double = startNode match {
    case Some(x) =>
      val firstJunction = x.findNextJunction().thisNode
      val pressureDifference = calculateResistanceFromNodeToNextJunction(firstJunction, pressureLossTable)
      val incomingFlow = calculateFlowAtPressureDifference(startNode, firePressure, regularPressure)
      val outgoingFlow = calculateFlowFromNodeToNextJunction(firstJunction)

      pressureDifference * Math.pow(incomingFlow / outgoingFlow, 2)

    case None => ???
  }
}
