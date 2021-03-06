package com.sootsafe.engine

import com.sootsafe.arithmetic.{Absolute, Expression, Sqrt, Value}
import com.sootsafe.model.{LinkedNode, PressureLossEntry}

object StepCalculation {
  def flowAtNextJunction(node: LinkedNode): Expression = {
    Value(node.nodeModule.ssInfo.capacity.get)
  }


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
  def calculateResistanceFromNodeToNextJunction(startNode: Option[LinkedNode],
                                                pressureLossTable: Map[Int, Double]): Double = startNode match {
    case Some(x) =>
      // A "trick" if the startNode is a junction itself
      calculateResistanceFromNodeToNextJunction(x.parent, pressureLossTable, pressureLossTable(x.nodeModule.key))
    case None => 0d
  }

  @scala.annotation.tailrec
  private def calculateResistanceFromNodeToNextJunction(startNode: Option[LinkedNode], pressureLossTable: Map[Int, Double], aggregator: Double = 0d): Double = {
    startNode match {
      case Some(n) if n.nodeModule.isJunction => aggregator
      case Some(n) => calculateResistanceFromNodeToNextJunction(n.parent, pressureLossTable, aggregator + pressureLossTable(n.nodeModule.key))
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
  def calculateFlowFromNodeToNextJunction(startNode: Option[LinkedNode]): Expression = startNode match {
    case Some(x) => calculateFlowFromNodeToNextJunction(x.parent, Value(x.nodeModule.ssInfo.capacity.getOrElse(0d)))
    case None => Value(0)
  }

  @scala.annotation.tailrec
  private def calculateFlowFromNodeToNextJunction(startNode: Option[LinkedNode], max: Value = Value(0)): Expression = startNode match {
    case Some(x) if x.nodeModule.isJunction => max
    case Some(x) => calculateFlowFromNodeToNextJunction(x.parent, Value(Math.max(max.calculate(), x.nodeModule.ssInfo.capacity.getOrElse(0d))))
    case None => max
  }

  /**
    * Boverket 1994:13 Appendix B, steg 6
    *
    * @param firePressure           The pressure (in Pascal) during fire (boverket schablon: 1000 Pa)
    * @param regularPressure        The pressure (in Pascal) during regular circumstances
    * @param aggregatedIncomingFlow The incoming flow (in l/s) to this node
    * @return The flow (in l/s) through this point
    */
  def calculateFlowAtPressureDifference(firePressure: Expression,
                                        regularPressure: Expression,
                                        aggregatedIncomingFlow: Expression = Value(0)): Expression = {
    Absolute(aggregatedIncomingFlow) * Sqrt(firePressure.toValue / regularPressure.toValue)
  }

  /**
    * Boverket 1994:13 Appendix B steg 7
    *
    * @param startNode         The node to start this calculation from
    * @param pressureLossTable The pressure loss data for this model
    * @param fireFlow          The incoming flow (in l/s) from the previous node (leading to the fire cell)
    * @param regularFlow       The regular flow (in l/s) from the previous node (leading to the fire cell)
    * @return The delta pressure (in Pascal) from this node, during a fire
    */
  def calculateAggregatedPressure(startNode: LinkedNode,
                                  pressureLossTable: Map[Int, Double],
                                  fireFlow: Expression,
                                  regularFlow: Expression): Expression = {
    val pressureDifference = calculateResistanceFromNodeToNextJunction(Some(startNode), pressureLossTable)
    val aggregatedRegularFlow_q = regularFlow.toValue
    Value(pressureDifference) * ((fireFlow.toValue / aggregatedRegularFlow_q) ^ Value(2))
  }
}
