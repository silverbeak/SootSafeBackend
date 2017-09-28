package com.sootsafe.calcutils

import com.sootsafe.model.SootSafeInfo


object VelocityCalculator {
  def velocity(ssInfo: SootSafeInfo): Double = {
    val area = AreaCalculator.area(ssInfo.dimension.diameter.getOrElse(0d))
    val flow = ssInfo.capacity.getOrElse(0d)
    flow / area
  }
}

object AreaCalculator {
  def area(diameter: Double): Double = Math.pow(diameter, 2) * Math.PI / 4
}