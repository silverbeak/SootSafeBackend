package com.sootsafe.engine

import com.sootsafe.model.{LinkedNode, NodeIterator}
import com.sootsafe.valuetable.ValueResolver

object FlowAndPressureHelper {

  /**
    * From the given node, in direction towards the outlet, create a table of nodes and pressure loss entries.
    * The pressure loss for each node is calculated using the supplied ValueResolver. (Not yet implemented)
    *
    * @param targetNode    The node from which to start the calculation.
    * @param valueResolver The value resolver to use when calculating the pressure loss
    * @return A Map with the node key as key, and the regular pressure loss as the value
    *         (Note, this is not the aggregated value, but the momentary value in the node)
    */
  def generateRegularPressureLossTable(targetNode: LinkedNode, valueResolver: ValueResolver): Map[Int, Double] = {
    new NodeIterator(Option(targetNode))
      .map(node => node.nodeModule.key -> node.nodeModule.ssInfo.pressureloss.getOrElse(0d))
      .toMap
  }

  /**
    * From the given node, calculate the aggregated pressure loss in each node in direction towards the outlet.
    *
    * @param targetNode The node from which to start the calculation. Most often, this will be the target fire cell.
    * @return A Map with the node key (as key) and the aggregated pressure loss as the value.
    */
  def generateAggregatedRegularPressureLossTable(targetNode: LinkedNode): Map[Int, Double] = {
    NodeIterator(targetNode)
      .foldLeft((Map[Int, Double](), 0d)) {
        case (aggregator, node) =>
          val pressureValue = node.nodeModule.ssInfo.pressureloss.get
          (
            aggregator._1 + (node.nodeModule.key -> (aggregator._2 + pressureValue)),
            aggregator._2 + pressureValue
          )
      }._1
  }

  /**
    * From the given node, calculate the pressure loss between all junctions
    *
    * @param targetNode The node to start the calculation (exclusive) from which to start the calculation (this will most likely always be the target fire node)
    * @return A Map with the node key (as key) and the pressure loss from the previous junction leading up to the node for the node with the given key.
    */
  def generateJunctionToJunctionPressureLossTable(targetNode: LinkedNode): Map[Int, Double] = {
    NodeIterator(targetNode)
      .foldLeft((Map[Int, Double](), 0d)) {
        case (aggregator, node) if node.nodeModule.isJunction =>
          val pressureValue = node.nodeModule.ssInfo.pressureloss.get
          (
            aggregator._1 + (node.nodeModule.key -> aggregator._2),
            pressureValue
          )
        case (aggregator, node) =>
          val pressureValue = node.nodeModule.ssInfo.pressureloss.get
          (
            aggregator._1,
            aggregator._2 + pressureValue
          )
      }._1
  }


  /**
    * Given a node, for all parent nodes (in direction towards the outlet), calculate the added regular flow.
    * The table will contain all nodes from the target node (which is often the target fire cell) to the outlet.
    * However, since only junctions (and fire cells) has any added flow, the value will only change for those node types
    *
    * @param targetNode The node from which to initiate the calculation. Most commonly, this will be the target fire cell
    * @return A Map with the node key (as key) and the added (regular) flow as the value
    */
  def generateRegularFlowTable(targetNode: LinkedNode): Map[Int, Double] = {
    // Not pretty. But we only want to calculate flow for cells, junctions and boxes
    val junctionTypes = Seq("fireCell", "tpipe", "box")

    NodeIterator(targetNode)
      .foldLeft((Map[Int, Double](), 0d)) {
        case (aggregator, node) if junctionTypes.contains(node.nodeModule.ssInfo.nodeType) =>
          // This is where we end up if the current node is a junction (or a fire cell)
          val flowValue = node.nodeModule.ssInfo.capacity.get
          (
            aggregator._1 + (node.nodeModule.key -> (flowValue - aggregator._2)),
            flowValue
          )

        case (aggregator, node) =>
          // If the current node is not a junction, just add the previously calculated value to the table (with the key of the current node)
          (
            aggregator._1 + (node.nodeModule.key -> aggregator._2),
            aggregator._2
          )

      }._1
  }
}