package com.sootsafe.model

import com.sootsafe.model.ModelResolver.IM

object ModelResolver {
  type IM = Option[LinkedNode] => Seq[LinkedNode]
}


case class LinkedNode(childResolver: IM, nodeModule: NodeModule, parent: Option[LinkedNode]) {
  lazy val children = childResolver(Some(this))

  def findNextJunction(startNode: Option[LinkedNode]): Option[LinkedNode] = startNode match {
    case Some(x) => innerFindNextJunction(x.parent)
    case None => None
  }

  private def innerFindNextJunction(startNode: Option[LinkedNode]): Option[LinkedNode] = startNode match {
    case Some(x) if x.nodeModule.isJunction => Some(x)
    case Some(x) => innerFindNextJunction(x.parent)
    case None => None
  }



  def locateTargetNode(): Option[LinkedNode] = {
    if (nodeModule.ssInfo.targetCell) Some(this)
    else children.flatMap(ln => ln.locateTargetNode()).headOption
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