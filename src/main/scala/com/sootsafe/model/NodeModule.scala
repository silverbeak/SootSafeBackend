package com.sootsafe.model

import com.sootsafe.calcutils.VelocityCalculator
import com.sootsafe.valuetable.ValueResolver

case class PressureLossEntry(id: Int, pressureLoss: Double)

trait PressureLossTrait {
  val rho: Double = 1.2

  def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule] = None): PressureLossEntry
}

trait NodeModule extends PressureLossTrait {
  val key: Int
  val ssInfo: SootSafeInfo
  val loc: String
  val angle: Option[String]
  val ports: List[Port]
  val isJunction: Boolean = false
}

case class NodeModuleBase(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  override def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule]) = PressureLossEntry(key, 0d)
}

case class Port(id: String, spot: String)

case class Bend(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  override def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule] = None): PressureLossEntry = {
    val v1 = VelocityCalculator.velocity(ssInfo)

    val zeta = valueResolver.componentPressureLoss(v1 * 1000)

    val pressureLoss = rho * Math.pow(v1 * 1000, 2) / 2 * zeta

    PressureLossEntry(key, pressureLoss)
  }
}

case class Outlet(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  override def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule]) = PressureLossEntry(key, 0d)
}

case class FireCell(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  override def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule]) = PressureLossEntry(key, 0d)
}

case class Pipe(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule]): PressureLossEntry = {
    val pressureLoss = ssInfo.dimension.length.getOrElse(0d) / 1000 * valueResolver.ductPressureLoss(this)
    PressureLossEntry(key, pressureLoss)
  }
}

case class AreaIncrement(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule] = None): PressureLossEntry = {
    val v1 = VelocityCalculator.velocity(originNode.get.ssInfo)
    val v2 = VelocityCalculator.velocity(ssInfo)
    val velocityFactor = v2 / v1

    val zeta = valueResolver.componentPressureLoss(velocityFactor)

    val pressureLoss = rho * Math.pow(v2 * 1000, 2) / 2 * zeta

    PressureLossEntry(key, pressureLoss)
  }
}

case class TPipe(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  override val isJunction: Boolean = true

  def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule]): PressureLossEntry = {
    val v2 = VelocityCalculator.velocity(originNode.get.ssInfo)
    val v1 = VelocityCalculator.velocity(ssInfo)
    val velocityFactor = v2 / v1

    val zeta = valueResolver.componentPressureLoss(velocityFactor)

    val pressureLoss = rho * Math.pow(v1 * 1000, 2) / 2 * zeta

    PressureLossEntry(key, pressureLoss)
  }
}

case class Box(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  override def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule]) = PressureLossEntry(key, 15d)
}

case class SootSafeInfo(nodeType: String, capacity: Option[Double], name: Option[String], comment: Option[String], pressureloss: Option[Double], dimension: Dimension, targetCell: Boolean = false)

case class Dimension(length: Option[Double], diameter: Option[Double])

case class Link(from: Int, to: Int, fid: String, tid: String)
