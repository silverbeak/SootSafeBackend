package com.sootsafe.engine

import com.sootsafe.model._
import com.sootsafe.valuetable.{PressureLossConstants, ValueResolver}

class PressureLoss(valueResolver: ValueResolver) extends PressureLossConstants {

  def calculatePressureLoss(startNode: LinkedNode, finalNode: LinkedNode): Seq[PressureLossEntry] = {
    iterativePressureLoss(startNode.parent, startNode, finalNode, Nil)
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

    node match {
      case pipe: Pipe =>
        val pressureLoss = pipe.ssInfo.pressureloss.getOrElse(
          valueResolver.ductPressureLoss(pipe)
        )
        PressureLossEntry(pipe.key, pressureLoss)

      case areaIncrement: AreaIncrement =>
        val pressureLoss = areaIncrement.ssInfo.pressureloss.getOrElse(
          valueResolver.areaIncrementPressureLoss(areaIncrement, originNode)
        )
        PressureLossEntry(areaIncrement.key, pressureLoss)

      case tPipe: TPipe =>
        val pressureLoss = tPipe.ssInfo.pressureloss.getOrElse(
          valueResolver.tPipePressureLoss(tPipe, originNode)
        )
        PressureLossEntry(node.key, pressureLoss)

      case bend: Bend =>
        val pressureLoss = bend.ssInfo.pressureloss.getOrElse(
          valueResolver.bendPressureLoss(bend)
        )
        PressureLossEntry(node.key, pressureLoss)

      case box: Box =>
        val pressureLoss = box.ssInfo.pressureloss.getOrElse(
          box.ssInfo.pressureloss.getOrElse(15d)
        )
        PressureLossEntry(node.key, pressureLoss)

      case n =>
        val pressureLoss = n.ssInfo.pressureloss.getOrElse(0d)
        PressureLossEntry(node.key, pressureLoss)
    }
  }
}
