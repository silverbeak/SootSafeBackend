package com.sootsafe.serializers

import java.io.InputStream

import com.sootsafe.model.{Model, Pipe}
import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.Serialization.read
import org.scalatest.{Matchers, WordSpecLike}

class NodeSerializerTest extends WordSpecLike with Matchers {

  implicit val formats: Formats = DefaultFormats + NodeSerializer

  "NodeSerializer" must {
    "return a Pipe instance" in {
      val stream: InputStream = getClass.getResourceAsStream("/nodes/SimplePipe.json")
      val pipeJson = scala.io.Source.fromInputStream(stream).mkString

      val model = read[Model](pipeJson)

      model.nodeDataArray.size should be(1)
      model.nodeDataArray.head.isInstanceOf[Pipe] should be(true)

    }
  }

}
