package com.sootsafe.backend.grpc

import com.sootsafe.firebase.subscriber.MessageSerializer
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass._
import org.json4s.DefaultFormats
import org.scalatest.{Matchers, WordSpecLike}

import scala.io.Source

class MessageSerializerTest extends WordSpecLike with Matchers {

  "MessageSerializerTest" must {

    "serialize json" in {
      val builder = ReleaseRateCalculatorOuterClass.ReleaseRateRequest.newBuilder
      val result = MessageSerializer.serializer[ReleaseRateRequest](FakeMessage.jsonMsg, builder)

      result.getCasNumber should be("74-86-2")
      result.getIsEvaporationFromPool should be(true)
      result.getGradeOfRelease should be(GradeOfRelease.Primary)
      result.getReleaseType should be(ReleaseType.DiffusiveJet)
      result.getVentilationAvailability should be(VentilationAvailability.Fair)

      result.getReleaseRateValues.getAdiabaticExpansion should be(13)
      result.getVentilationVelocityValues.getObstructed should be(Obstruction.Unobstructed)
    }

    "serialize map" in {
      import org.json4s.native.Serialization.write

      implicit val formats: DefaultFormats.type = DefaultFormats

      val msg = FakeMessage.mapMsg
      val json = write(msg)

      val builder = ReleaseRateCalculatorOuterClass.ReleaseRateRequest.newBuilder
      val result = MessageSerializer.serializer[ReleaseRateRequest](json, builder)

      result.getCasNumber should be("74-86-2")
      result.getIsEvaporationFromPool should be(true)
      result.getGradeOfRelease should be(GradeOfRelease.Primary)
      result.getReleaseType should be(ReleaseType.DiffusiveJet)
      result.getVentilationAvailability should be(VentilationAvailability.Fair)
      result.getReleaseRateValues.getVolumetricGasFlowRate should be(1)
      result.getReleaseRateValues.getSafetyFactor should be(2.3)
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
    "releaseRateValues" -> Map(
      "volumetricGasFlowRate" -> 1,
      "safetyFactor" -> "2.3"
    ),

    "isIndoors" -> true,

    "bgConcentrationValues" -> Map(),

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