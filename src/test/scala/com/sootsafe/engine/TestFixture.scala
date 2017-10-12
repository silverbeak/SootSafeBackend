package com.sootsafe.engine

import java.io.InputStream

import com.sootsafe.model.{LinkedNode, Model, ModelBuilder}
import com.sootsafe.serializers.NodeSerializer
import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.Serialization.read

trait TestFixture {
  lazy val linkedModel: LinkedNode = createLinkedModel("/defaultTestData.json")

  implicit val formats: Formats = DefaultFormats + NodeSerializer

  private def createLinkedModel(fileName: String): LinkedNode = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    val defaultJson = scala.io.Source.fromInputStream(stream).mkString
    val model = read[Model](defaultJson)
    new ModelBuilder(model).buildModel()
  }
}
