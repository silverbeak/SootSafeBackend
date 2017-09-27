package com.sootsafe.engine

import com.sootsafe._
import com.sootsafe.valuetable.ValueResolver

class PressureLoss(valueResolver: ValueResolver) {

  private def locateTargetNode(node: Option[LinkedNode]): Option[LinkedNode] = {
    node.flatMap { n =>
      if (n.nodeModule.ssInfo.targetCell) Some(n)
      else n.children.flatMap(ln => locateTargetNode(Some(ln))).headOption
    }
  }

  def calculatePressureLoss(node: LinkedNode): Double = {
    println(s"Capacity: ${node.nodeModule.ssInfo.capacity} and ${node.nodeModule.ssInfo.nodeType}")
    val targetNode = locateTargetNode(Some(node))
    iterativePressureLoss(targetNode, node, node, 0d)
  }

  private def findRoots(node: LinkedNode, exclude: Seq[LinkedNode]): Seq[LinkedNode] = {
    node.children.filterNot(exclude.contains).flatMap { n =>
      n.children match {
        case Nil => Seq(n)
        case _ => findRoots(n, Nil)
      }
    }
  }

  private def iterativePressureLoss(node: Option[LinkedNode], originNode: LinkedNode, stopNode: LinkedNode, aggregator: Double): Double = {
    node match {
      case Some(targetNode) if targetNode == stopNode =>
        aggregator

      case Some(targetNode) =>
        val pressureLoss = singleComponentPressureLoss(targetNode.nodeModule, originNode.nodeModule)

        findRoots(targetNode, Seq(originNode)) match {
          case Nil => iterativePressureLoss(targetNode.parent, targetNode, stopNode, aggregator + pressureLoss)
          case roots =>
            val rootsPressure = roots.foldLeft(0d) {
              case (agg, n) => iterativePressureLoss(Some(n), targetNode, targetNode, agg)
            }
            val it = iterativePressureLoss(targetNode.parent, targetNode, stopNode, aggregator + rootsPressure)
            println(s"IT is ${it}")
            it
        }
      case None => aggregator
    }
  }

  private def singleComponentPressureLoss(node: NodeModule, originNode: NodeModule): Double = {
    val rho = 1.2

    node.ssInfo.nodeType match {
      case "pipe" =>
        node.ssInfo.dimension.length.getOrElse(0d) / 1000 * valueResolver.ductPressureLoss(node)
      case "areaIncrement" =>
        val v1 = VelocityCalculator.velocity(originNode.ssInfo)
        val v2 = VelocityCalculator.velocity(node.ssInfo)
        val velocityFactor = v2/v1

        val zeta = valueResolver.componentPressureLoss(velocityFactor)

        rho * Math.pow(v2 * 1000, 2) / 2 * zeta

      case "t-pipe" =>
        val v2 = VelocityCalculator.velocity(originNode.ssInfo)
        val v1 = VelocityCalculator.velocity(node.ssInfo)
        val velocityFactor = v2/v1

        val zeta = valueResolver.componentPressureLoss(velocityFactor)

        rho * Math.pow(v1 * 1000, 2) / 2 * zeta

      case "bend" =>
        val v1 = VelocityCalculator.velocity(node.ssInfo)

        val zeta = valueResolver.componentPressureLoss(v1 * 1000)

        rho * Math.pow(v1 * 1000, 2) / 2 * zeta

      case "box" => 15d

      case _ => 0d
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