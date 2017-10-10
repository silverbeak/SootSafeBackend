package com.sootsafe.serializers

import com.sootsafe.model._
import com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.Node
import com.sootsafe.model.{SootSafeInfo => SSInfo}

import scala.util.{Failure, Success, Try}

object GRPCSerializer {

  def deserialize(node: Node): NodeModule = {
    val nodeType: String = Try(node.getSsInfo.getNodeType) match {
      case Success(t) => t
      case Failure(e) => throw new Exception(s"Model $node has invalid SootSafeInfo or NodeType value", e)
    }

    val dimension: com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.Dimension => Dimension = { dim =>
      Dimension(
        Option(dim.getLength),
        Option(dim.getDiameter)
      )
    }

    val ssInfo: com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.SootSafeInfo => SSInfo = { info =>
      SSInfo(
        info.getNodeType,
        Option(info.getCapacity),
        Option(info.getName),
          Option(info.getComment),
        Option(0d),
        dimension(info.getDimension),
        info.getTargetCell
      )
    }
    val convertedInfo = ssInfo(node.getSsInfo)

    nodeType match {
      case "bend" => Bend(node.getKey, convertedInfo)
      case "outlet" => Outlet(node.getKey, convertedInfo)
      case "fireCell" => FireCell(node.getKey, convertedInfo)
      case "pipe" => Pipe(node.getKey, convertedInfo)
      case "t-pipe" => TPipe(node.getKey, convertedInfo)
      case "areaIncrement" => AreaIncrement(node.getKey, convertedInfo)
      case "box" => Box(node.getKey, convertedInfo)
      case _ => NodeModuleBase(node.getKey, convertedInfo)
    }
  }

  def deserialize(link: com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.Link): Link = {
    Link(link.getFrom, link.getTo, link.getFid, link.getTid)
  }

}
