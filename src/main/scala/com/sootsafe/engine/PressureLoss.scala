package com.sootsafe.engine

import com.sootsafe.model._
import com.sootsafe.valuetable.ValueResolver

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

  private def singleComponentPressureLoss(node: PressureLossTrait, originNode: NodeModule): PressureLossEntry = {
    node.pressureLoss(valueResolver, Option(originNode))
  }

}
