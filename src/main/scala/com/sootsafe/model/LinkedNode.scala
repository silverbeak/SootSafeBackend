package com.sootsafe.model

import com.sootsafe.model.ModelResolver.IM

object ModelResolver {
  type IM = Option[LinkedNode] => Seq[LinkedNode]
}

case class AscendingNodePair(thisNode: Option[LinkedNode], previousNode: Option[LinkedNode])

case class LinkedNode(childResolver: IM, nodeModule: NodeModule, parent: Option[LinkedNode]) {
  lazy val children = childResolver(Some(this))

  def findNextJunction(): AscendingNodePair = {
    innerFindNextJunction(AscendingNodePair(parent, Some(this)))
  }

  private def innerFindNextJunction(ascendingNodePair: AscendingNodePair): AscendingNodePair = ascendingNodePair.thisNode match {
    case Some(x) if x.nodeModule.isJunction => ascendingNodePair
    case Some(x) => innerFindNextJunction(AscendingNodePair(x.parent, ascendingNodePair.thisNode))
    case None => AscendingNodePair(None, None)
  }



  def locateTargetNode(): Option[LinkedNode] = {
    if (nodeModule.ssInfo.targetCell) Some(this)
    else children.flatMap(ln => ln.locateTargetNode()).headOption
  }

  def locateOutletNode(): Option[LinkedNode] = {
    nodeModule.ssInfo.nodeType match {
      case "outlet" => Some(this)
      case _ if parent.nonEmpty => parent.get.locateOutletNode()
      case _ => None
    }
  }

  def findRoots(exclude: Seq[LinkedNode]): Seq[LinkedNode] = {
    children.filterNot(exclude.contains).flatMap { n =>
      n.children match {
        case Nil => Seq(n)
        case _ => n.findRoots(Nil)
      }
    }
  }
}