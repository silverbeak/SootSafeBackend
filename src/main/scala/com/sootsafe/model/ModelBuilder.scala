package com.sootsafe.model

class ModelBuilder(model: Model) {

  type ErrorMessage = String

  private def findOutlet(): Option[NodeModule] = model.nodeDataArray.find(_.ssInfo.nodeType == "outlet")

  def buildModel(): Either[LinkedNode, ErrorMessage] = {
    findOutlet() match {
      case None =>
        Right("Model does not contain outlet")
      case Some(outlet) =>
        Left(linkNested(outlet, Nil)(Some(LinkedNode((_) => Nil, outlet, None))))
    }
  }

  private def linkNested(nodeModule: NodeModule, alreadyBoundNodes: Seq[Int])(parentLinkedNode: Option[LinkedNode]): LinkedNode = {
    ModelBuilder.findLinkedNodes(model, nodeModule, alreadyBoundNodes) match {
      case Nil =>
        LinkedNode((_) => Nil, nodeModule, parentLinkedNode)
      case links =>
        val subLinks = links.map(n => linkNested(n, alreadyBoundNodes :+ nodeModule.key)(_))

        // This part is tricky, and I don't really like it, but it works
        // The problem is that LinkedNode contains circular referencing,
        // so a LinkedNode cannot be "completed" without both parent and children have been defined.
        // This is of course not possible, so instead we introduce a function to retrieve the children "later".
        // Hence the name "childResolver"
        val childResolver = (o: Option[LinkedNode]) => subLinks.map(sl => sl(o))
        LinkedNode(childResolver, nodeModule, parentLinkedNode)
    }
  }

}

object ModelBuilder {
  private def findLinks(model: Model, node: NodeModule, alreadyBoundNodes: Seq[Int]): List[Link] =
    model.linkDataArray
      .filter(l => l.from == node.key || l.to == node.key)
      .filterNot(l => alreadyBoundNodes.contains(l.to) || alreadyBoundNodes.contains(l.from))

  private def findLinkedNodes(model: Model, node: NodeModule, alreadyBoundNodes: Seq[Int]): List[NodeModule] =
    findLinks(model, node, alreadyBoundNodes)
      .flatMap(link => model.nodeDataArray.filter(n => n.key == link.from || n.key == link.to))
      .filter(_.key != node.key)
}
