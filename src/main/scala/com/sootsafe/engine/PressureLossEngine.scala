package com.sootsafe.engine

import com.sootsafe.model.LinkedNode
import com.sootsafe.valuetable.ValueResolver

trait PressureLossEngine {
  def calculatePressureLoss(linkedModel: LinkedNode,
                            initialRegularPressure: Option[Double] = None,
                            initialFirePressure: Double,
                            valueResolver: ValueResolver): Either[Seq[FlowAndPressure], String]
}
