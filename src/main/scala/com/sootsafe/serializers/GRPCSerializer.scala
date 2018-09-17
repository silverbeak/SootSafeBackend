package com.sootsafe.serializers

import com.sootsafe.model.{SootSafeInfo => SSInfo, _}
import com.sootsafe.server.calculator.SootSafeCalculator.{Field, Node}

import scala.reflect.ClassTag

object GRPCSerializer {

  private def findFieldValue[T: ClassTag](fields: Map[String, Field], name: String)(implicit ct: ClassTag[T]): Option[T] = {

    def findField(fields: Map[String, Field]): Option[Field] = fields.get(name) match {
      case Some(field) => Some(field)
//      case None if fields.exists() =>
//        val fs = for {
//          f <- fields.filter(_.getChildrenCount > 0)
//          found <- findField(f.getChildrenMap.values().toSeq)
//        } yield found
//        fs.headOption
      case None => None
    }

    val found = findField(fields)

    (ct.runtimeClass, found) match {
      case (x, Some(field)) if x == classOf[Double] => Option(field.value.toDouble.asInstanceOf[T])
      case (x, Some(field)) if x == classOf[String] => Option(field.value.asInstanceOf[T])
      case (x, Some(field)) if x == classOf[Boolean] => Option(field.value.toBoolean.asInstanceOf[T])
      case (x, Some(field)) if x == classOf[Dimension] =>
        val length = findFieldValue[Double](field.children, "length")
        val diameter = findFieldValue[Double](field.children, "diameter")
        Option(Dimension(length, diameter).asInstanceOf[T])
      case _ => None
    }
  }

  def deserialize(node: Node): NodeModule = {
    val nodeType = node.getType.name

    val ssInfo: Map[String, Field] => SSInfo = { fields =>
      SSInfo(
        nodeType,
        findFieldValue[Double](fields, "capacity"),
        findFieldValue[String](fields, "name"),
        findFieldValue[String](fields, "comment"),
        findFieldValue[Double](fields, "pressureLoss"),
        findFieldValue[Dimension](fields, "dimension").getOrElse(Dimension(None, None)),
        findFieldValue[Boolean](fields, "targetCell").getOrElse(false)
      )
    }

    val convertedInfo = ssInfo(node.fields)

    nodeType match {
      case "bend" => Bend(node.key, convertedInfo)
      case "outlet" => Outlet(node.key, convertedInfo)
      case "fireCell" => FireCell(node.key, convertedInfo)
      case "pipe" => Pipe(node.key, convertedInfo)
      case "tpipe" => TPipe(node.key, convertedInfo)
      case "areaIncrement" => AreaIncrement(node.key, convertedInfo)
      case "box" => Box(node.key, convertedInfo)
      case _ => NodeModuleBase(node.key, convertedInfo)
    }
  }

  def deserialize(link: com.sootsafe.server.calculator.SootSafeCalculator.Link): com.sootsafe.model.Link = {
    com.sootsafe.model.Link(link.from, link.to, link.fid, link.tid)
  }

}
