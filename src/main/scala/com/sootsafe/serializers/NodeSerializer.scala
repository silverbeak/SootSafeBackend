package com.sootsafe.serializers

import com.sootsafe.model._
import org.json4s.CustomSerializer
import org.json4s.JsonAST.{JField, JObject, JString}

case object NodeSerializer extends CustomSerializer[NodeModule](implicit format => ({
  case obj: JObject =>

    val nodeType: Seq[String] = for {
      JObject(child) <- obj
      JField("ssInfo", JObject(ssInfo)) <- child
      JField("nodeType", JString(nodeType))  <- ssInfo
    } yield nodeType

    nodeType.headOption match {
      case Some("bend") => obj.extract[Bend]
      case Some("outlet") => obj.extract[Outlet]
      case Some("fireCell") => obj.extract[FireCell]
      case Some("pipe") => obj.extract[Pipe]
      case Some("t-pipe") => obj.extract[TPipe]
      case Some("areaIncrement") => obj.extract[AreaIncrement]
      case Some("box") => obj.extract[Box]
      case _ => obj.extract[NodeModuleBase]
    }
}, {
  case _ => ???
}))
