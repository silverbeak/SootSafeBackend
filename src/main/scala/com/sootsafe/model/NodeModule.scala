package com.sootsafe.model

import com.sootsafe.valuetable.ValueResolver

case class PressureLossEntry(id: Int, pressureLoss: Double)

trait PressureLossTrait {
  val rho: Double = 1.2

  def pressureLoss(valueResolver: ValueResolver, originNode: Option[NodeModule] = None): PressureLossEntry
}

trait NodeModule {
  val key: Int
  val ssInfo: SootSafeInfo
  val loc: String
  val angle: Option[String]
  val ports: List[Port]
  val isJunction: Boolean = false
}

case class NodeModuleBase(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class Port(id: String, spot: String)

case class Bend(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class Outlet(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class FireCell(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class Pipe(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class SootSafeInfo(nodeType: String, capacity: Option[Double], name: Option[String], comment: Option[String], pressureloss: Option[Double], dimension: Dimension, targetCell: Boolean = false)

case class Dimension(length: Option[Double], diameter: Option[Double])

case class Link(from: Int, to: Int, fid: String, tid: String)

case class AreaIncrement(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class TPipe(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  override val isJunction: Boolean = true
}

case class Box(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule {
  override val isJunction: Boolean = true
}

