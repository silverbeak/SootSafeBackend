package com.sootsafe.backend.grpc

import com.sootsafe.server.calculator.AtexCalculator._
import org.json4s.DefaultFormats
import org.scalatest.{Matchers, WordSpecLike}
import scalapb.json4s.JsonFormat

import scala.io.Source

class MessageSerializerTest extends WordSpecLike with Matchers {

  "MessageSerializerTest" must {

    "serialize json" in {
      val result = JsonFormat.fromJsonString[AtexRequest](FakeMessage.jsonMsg)

      val fields = result.fields.get

      fields.casNumber should be("74-86-2")
      fields.isEvaporationFromPool should be(true)
      fields.gradeOfRelease should be(GradeOfRelease.Primary)
      fields.releaseType should be(ReleaseType.DiffusiveJet)
      fields.ventilationAvailability should be(VentilationAvailability.Fair)

      fields.getReleaseRate.adiabaticExpansion should be(13)
      fields.getVentilationVelocityValues.obstructed should be(Obstruction.Unobstructed)
    }

    "serialize map" in {
      import org.json4s.native.Serialization.write

      implicit val formats: DefaultFormats.type = DefaultFormats

      val msg = FakeMessage.mapMsg
      val json = write(msg)

      val result = JsonFormat.fromJsonString[AtexRequest](json)

      val fields = result.fields.get

      fields.casNumber should be("74-86-2")
      fields.isEvaporationFromPool should be(true)
      fields.gradeOfRelease should be(GradeOfRelease.Primary)
      fields.releaseType should be(ReleaseType.DiffusiveJet)
      fields.ventilationAvailability should be(VentilationAvailability.Fair)
      fields.releaseRate.get.volumetricGasFlowRate should be(1)
      fields.releaseRate.get.safetyFactor should be(2.3)
      fields.ventilationVelocityValues.get.obstructed should be(Obstruction.Unobstructed)
    }
  }
}

object FakeMessage {
  val mapMsg = Map(
    "key" -> 3,
    "isGasCalculation" -> true,
    "performReleaseCalculation" -> false,
    "hasReleaseRateInKgPerSecond" -> true,
    "isEvaporationFromPool" -> true,
    "releaseRate" -> Map(
      "volumetricGasFlowRate" -> 1,
      "safetyFactor" -> 2.3,
      "gasDensity" -> 4.44
    ),

    "isIndoors" -> true,

    "backgroundConcentration" -> Map(),

    "gradeOfRelease" -> "Primary",
    "releaseType" -> "DiffusiveJet",

    "ventilationVelocityValues" -> Map(
      "elevation" -> 1,
      "obstructed" -> "Unobstructed"
    ),
    "ventilationAvailability" -> "Fair",

    "casNumber" -> "74-86-2"
  )

  val jsonMsg: String = Source.fromURL(getClass.getResource("/json/test1.json")).mkString
}