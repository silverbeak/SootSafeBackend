package com.sootsafe

import org.json4s.CustomSerializer
import org.json4s.JsonAST.JObject

case object NodeSerializer extends CustomSerializer[NodeModule](implicit format => ({
  case obj: JObject =>
    obj.values.getOrElse("ssInfo.nodeType", "").toString.toLowerCase match {
      case "angle" => obj.extract[Angle]
      case "outlet" => obj.extract[Outlet]
      case "firecell" => obj.extract[FireCell]
      case "pipe" => obj.extract[Pipe]
      case _ => obj.extract[NodeModuleBase]
    }
}, {
  case _ => ???
}))
