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

      result.casNumber should be("74-86-2")
      result.isEvaporationFromPool should be(true)
      result.gradeOfRelease should be(GradeOfRelease.Primary)
      result.releaseType should be(ReleaseType.DiffusiveJet)
      result.ventilationAvailability should be(VentilationAvailability.Fair)

      result.getReleaseRate.adiabaticExpansion should be(13)
      result.getVentilationVelocityValues.obstructed should be(Obstruction.Unobstructed)
    }

    "serialize map" in {
      import org.json4s.native.Serialization.write

      implicit val formats: DefaultFormats.type = DefaultFormats

      val msg = FakeMessage.mapMsg
      val json = write(msg)

      val result = JsonFormat.fromJsonString[AtexRequest](json)

      result.casNumber should be("74-86-2")
      result.isEvaporationFromPool should be(true)
      result.gradeOfRelease should be(GradeOfRelease.Primary)
      result.releaseType should be(ReleaseType.DiffusiveJet)
      result.ventilationAvailability should be(VentilationAvailability.Fair)
      result.releaseRate.get.volumetricGasFlowRate should be(1)
      result.releaseRate.get.safetyFactor should be(2.3)
      result.ventilationVelocityValues.get.obstructed should be(Obstruction.Unobstructed)
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