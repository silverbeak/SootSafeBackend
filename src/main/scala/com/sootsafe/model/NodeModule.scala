package com.sootsafe.model

case class PressureLossEntry(id: Int, pressureLoss: Double)

trait NodeModule {
  val key: Int
  val ssInfo: SootSafeInfo
  val isJunction: Boolean = false
}

case class NodeModuleBase(key: Int, ssInfo: SootSafeInfo) extends NodeModule

case class Bend(key: Int, ssInfo: SootSafeInfo) extends NodeModule

case class Outlet(key: Int, ssInfo: SootSafeInfo) extends NodeModule

case class FireCell(key: Int, ssInfo: SootSafeInfo) extends NodeModule

case class Pipe(key: Int, ssInfo: SootSafeInfo) extends NodeModule

case class Dimension(length: Option[Double], diameter: Option[Double])

case class Link(from: Int, to: Int, fid: String, tid: String)

case class AreaIncrement(key: Int, ssInfo: SootSafeInfo) extends NodeModule

case class TPipe(key: Int, ssInfo: SootSafeInfo) extends NodeModule {
  override val isJunction: Boolean = true
}

case class Box(key: Int, ssInfo: SootSafeInfo) extends NodeModule {
  override val isJunction: Boolean = true
}

case class SootSafeInfo(nodeType: String,
                        capacity: Option[Double],
                        name: Option[String],
                        comment: Option[String],
                        pressureloss: Option[Double],
                        dimension: Dimension,
                        targetCell: Boolean = false)
