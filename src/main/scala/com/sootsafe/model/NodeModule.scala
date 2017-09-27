package com.sootsafe.model

trait NodeModule {
  val key: Int
  val ssInfo: SootSafeInfo
  val loc: String
  val angle: Option[String]
  val ports: List[Port]
}

case class NodeModuleBase(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class Port(id: String, spot: String)

case class Angle(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class Outlet(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class FireCell(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class Pipe(key: Int, ssInfo: SootSafeInfo, loc: String, angle: Option[String], ports: List[Port]) extends NodeModule

case class SootSafeInfo(nodeType: String, capacity: Option[Double], name: Option[String], comment: Option[String], pressureloss: Option[Double], dimension: Dimension, targetCell: Boolean = false)

case class Dimension(length: Option[Double], diameter: Option[Double])

case class Link(from: Int, to: Int, fid: String, tid: String)
