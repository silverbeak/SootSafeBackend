package com.sootsafe.backend.grpc

import com.sootsafe.firebase.subscriber.MessageSerializer
import com.sootsafe.server.calculator.AtexCalculatorOuterClass
import com.sootsafe.server.calculator.AtexCalculatorOuterClass._
import org.json4s.DefaultFormats
import org.scalatest.{Matchers, WordSpecLike}

import scala.io.Source

class MessageSerializerTest extends WordSpecLike with Matchers {

  "MessageSerializerTest" must {

    "serialize json" in {
      val builder = AtexCalculatorOuterClass.AtexRequest.newBuilder
      val result = MessageSerializer.serializer[AtexRequest](FakeMessage.jsonMsg, builder)

      result.getCasNumber should be("74-86-2")
      result.getIsEvaporationFromPool should be(true)
      result.getGradeOfRelease should be(GradeOfRelease.Primary)
      result.getReleaseType should be(ReleaseType.DiffusiveJet)
      result.getVentilationAvailability should be(VentilationAvailability.Fair)

      result.getReleaseRate.getAdiabaticExpansion should be(13)
      result.getVentilationVelocityValues.getObstructed should be(Obstruction.Unobstructed)
    }

    "serialize map" in {
      import org.json4s.native.Serialization.write

      implicit val formats: DefaultFormats.type = DefaultFormats

      val msg = FakeMessage.mapMsg
      val json = write(msg)

      val builder = AtexCalculatorOuterClass.AtexRequest.newBuilder
      val result = MessageSerializer.serializer[AtexRequest](json, builder)

      result.getCasNumber should be("74-86-2")
      result.getIsEvaporationFromPool should be(true)
      result.getGradeOfRelease should be(GradeOfRelease.Primary)
      result.getReleaseType should be(ReleaseType.DiffusiveJet)
      result.getVentilationAvailability should be(VentilationAvailability.Fair)
      result.getReleaseRate.getVolumetricGasFlowRate should be(1)
      result.getReleaseRate.getSafetyFactor should be(2.3)
      result.getVentilationVelocityValues.getObstructed should be(Obstruction.Unobstructed)
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
      "safetyFactor" -> "2.3",
      "gasDensity" -> "4.44"
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