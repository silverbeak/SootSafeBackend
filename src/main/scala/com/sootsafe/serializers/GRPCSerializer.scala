package com.sootsafe.serializers

import com.sootsafe.model.{SootSafeInfo => SSInfo, _}
import com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.{Field, Node}

import scala.collection.JavaConversions._
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
      case (x, Some(field)) if x == classOf[Double] => Option(field.getValue.toDouble.asInstanceOf[T])
      case (x, Some(field)) if x == classOf[String] => Option(field.getValue.asInstanceOf[T])
      case (x, Some(field)) if x == classOf[Boolean] => Option(field.getValue.toBoolean.asInstanceOf[T])
      case (x, Some(field)) if x == classOf[Dimension] =>
        val length = findFieldValue[Double](field.getChildrenMap.toMap[String, Field], "length")
        val diameter = findFieldValue[Double](field.getChildrenMap.toMap[String, Field], "diameter")
        Option(Dimension(length, diameter).asInstanceOf[T])
      case _ => None
    }
  }

  def deserialize(node: Node): NodeModule = {
    val nodeType = node.getType.getName

    val ssInfo: Map[String, com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.Field] => SSInfo = { fields =>
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

    val convertedInfo = ssInfo(node.getFieldsMap.toMap[String, Field])

    nodeType match {
      case "bend" => Bend(node.getKey, convertedInfo)
      case "outlet" => Outlet(node.getKey, convertedInfo)
      case "fireCell" => FireCell(node.getKey, convertedInfo)
      case "pipe" => Pipe(node.getKey, convertedInfo)
      case "tpipe" => TPipe(node.getKey, convertedInfo)
      case "areaIncrement" => AreaIncrement(node.getKey, convertedInfo)
      case "box" => Box(node.getKey, convertedInfo)
      case _ => NodeModuleBase(node.getKey, convertedInfo)
    }
  }

  def deserialize(link: com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.Link): Link = {
    Link(link.getFrom, link.getTo, link.getFid, link.getTid)
  }

}
