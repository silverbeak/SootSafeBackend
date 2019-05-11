package com.sootsafe.engine

import com.sootsafe.model._
import com.sootsafe.valuetable.{PressureLossConstants, ValueResolver}

class PressureLoss(valueResolver: ValueResolver) extends PressureLossConstants {

  def calculatePressureLoss2(sourceNode: LinkedNode): Map[Int, Double] = {
    new NodeIterator(Option(sourceNode))
      .map(node => node.nodeModule.key -> node.nodeModule.ssInfo.pressureloss.getOrElse(0d))
      .toMap
  }
}
