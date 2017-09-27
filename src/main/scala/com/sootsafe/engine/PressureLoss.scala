package com.sootsafe.engine

import com.sootsafe._
import com.sootsafe.model.LinkedNode
import com.sootsafe.valuetable.ValueResolver

case class PressureLossEntry(id: Int, pressureLoss: Double)

class PressureLoss(valueResolver: ValueResolver) {

  def calculatePressureLoss(node: LinkedNode): Seq[PressureLossEntry] = {
    val targetNode = node.locateTargetNode()
    iterativePressureLoss(targetNode, node, node, Nil)
  }

  private def iterativePressureLoss(node: Option[LinkedNode],
                                    originNode: LinkedNode,
                                    stopNode: LinkedNode,
                                    aggregator: Seq[PressureLossEntry]): Seq[PressureLossEntry] = {
    node match {
      case Some(targetNode) if targetNode == stopNode =>
        aggregator

      case Some(targetNode) =>
        val pressureLoss = singleComponentPressureLoss(targetNode.nodeModule, originNode.nodeModule)

        targetNode.findRoots(Seq(originNode)) match {
          case Nil => iterativePressureLoss(targetNode.parent, targetNode, stopNode, aggregator :+ pressureLoss)
          case roots =>
            // TODO: Not sure if this is how it works... don't think so
            val rootsPressure = roots.foldLeft(Seq[PressureLossEntry]()) {
              case (agg, n) => iterativePressureLoss(Some(n), targetNode, targetNode, agg)
            }
            iterativePressureLoss(targetNode.parent, targetNode, stopNode, aggregator ++ rootsPressure)
        }
      case None => aggregator
    }
  }

  private def singleComponentPressureLoss(node: NodeModule, originNode: NodeModule): PressureLossEntry = {
    val rho = 1.2

    node.ssInfo.nodeType match {
      case "pipe" =>
        val pressureLoss = node.ssInfo.dimension.length.getOrElse(0d) / 1000 * valueResolver.ductPressureLoss(node)
        PressureLossEntry(node.key, pressureLoss)
      case "areaIncrement" =>
        val v1 = VelocityCalculator.velocity(originNode.ssInfo)
        val v2 = VelocityCalculator.velocity(node.ssInfo)
        val velocityFactor = v2/v1

        val zeta = valueResolver.componentPressureLoss(velocityFactor)

        val pressureLoss = rho * Math.pow(v2 * 1000, 2) / 2 * zeta

        PressureLossEntry(node.key, pressureLoss)

      case "t-pipe" =>
        val v2 = VelocityCalculator.velocity(originNode.ssInfo)
        val v1 = VelocityCalculator.velocity(node.ssInfo)
        val velocityFactor = v2/v1

        val zeta = valueResolver.componentPressureLoss(velocityFactor)

        val pressureLoss = rho * Math.pow(v1 * 1000, 2) / 2 * zeta

        PressureLossEntry(node.key, pressureLoss)

      case "bend" =>
        val v1 = VelocityCalculator.velocity(node.ssInfo)

        val zeta = valueResolver.componentPressureLoss(v1 * 1000)

        val pressureLoss = rho * Math.pow(v1 * 1000, 2) / 2 * zeta

        PressureLossEntry(node.key, pressureLoss)

      case "box" => PressureLossEntry(node.key, 15d)

      case _ => PressureLossEntry(node.key, 0d)
    }
  }

}

object VelocityCalculator {
  def velocity(ssInfo: SootSafeInfo): Double = {
    val area = AreaCalculator.area(ssInfo.dimension.diameter.getOrElse(0d))
    val flow = ssInfo.capacity.getOrElse(0d)
    flow/area
  }
}

object AreaCalculator {
  def area(diameter: Double): Double = Math.pow(diameter, 2) * Math.PI / 4
}