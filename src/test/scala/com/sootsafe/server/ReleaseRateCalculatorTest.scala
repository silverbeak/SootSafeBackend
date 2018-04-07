package com.sootsafe.server

import com.sootsafe.backend.grpc.FakeMessage
import com.sootsafe.engine.zone.ReleaseRateCalculator
import com.sootsafe.firebase.subscriber.MessageSerializer
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass.{ReleaseRateRequest, ReleaseRateValues}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpecLike}

class ReleaseRateCalculatorTest extends WordSpecLike with Matchers with BeforeAndAfterEach {

  private var baseRequestValues: ReleaseRateValues.Builder = _

  override def beforeEach(): Unit = {
    baseRequestValues = ReleaseRateValues.newBuilder()
      .setVolumetricGasFlowRate(.44)
      .setSafetyFactor(33.4)
      .setLowerFlammableLimit(.1)
      .setMassReleaseRate(9)
      .setMolarMass(33.3)
      .setGasDensity(4.44)
      .setDischargeCoefficient(7)
      .setCrossSectionArea(.7)
      .setPressureDifference(44)
      .setPoolSurfaceArea(3)
      .setWindSpeed(3)
      .setAbsoluteTemperature(3)
      .setAdiabaticExpansion(55)
      .setAtmosphericPressure(33)
      .setCriticalGasPressure(1000)
      .setCompressibilityFactor(11)


    //  Qg: Expression,                     .44   volumetricGasFlowRate
    //  k: Expression,                      33    safetyFactor
    //  lfl: Expression,                    0.1   lowerFlammableLimit
    //  Wg: Expression,                     9     massReleaseRate
    //  M: Expression,                      33.3  molarMass
    //  rhoG: Expression,                   0     gasDensity
    //  Cd: Expression,                     7     dischargeCoefficient
    //  S: Expression,                      0.7   crossSectionArea
    //  deltaP: Expression,                 44    pressureDifference
    //  Ap: Expression,                     3     poolSurfaceArea
    //  uw: Expression,                     3     windSpeed
    //  T: Expression,                      3     absoluteTemperature
    //  gma: Expression,                    55    adiabaticExpansion
    //  pa: Expression,                     33    atmosphericPressure
    //  criticalPressure: Expression,       1000  criticalGasPressure
    //  compressibilityFactor: Expression   11    compressibilityFactor
  }


  "Calculator" must {
    "handle request" in {
      val request = ReleaseRateRequest
        .newBuilder()
        .setReleaseRateValues(baseRequestValues)
        .setCasNumber("74-86-2")
        .build()


      ReleaseRateCalculator.handleRequest(request) match {
        case Right(errorString) => fail(errorString)
        case Left(result) =>
          result.getReleaseRateResult.getKey should be(request.getKey)
          result.getReleaseRateResult.getReleaseCharacter should be(0.6068943194691696)
      }
    }

    "handle request based on json" in {
      val builder = ReleaseRateCalculatorOuterClass.ReleaseRateRequest.newBuilder
      val request = MessageSerializer.serializer[ReleaseRateRequest](FakeMessage.jsonMsg, builder)

      ReleaseRateCalculator.handleRequest(request) match {
        case Right(errorString) => fail(errorString)
        case Left(result) =>
          result.getReleaseRateResult.getKey should be(request.getKey)
          result.getReleaseRateResult.getReleaseCharacter should be(0.1111111111111111)
      }
    }

    "return an expression [!performRelease, !isGas, hasReleaseRate, !isEvaporation]" in {

      val requestForGas = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(false)
        .setIsGasCalculation(false)
        .setHasReleaseRateInKgPerSecond(false)
        .setIsEvaporationFromPool(false)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val result = ReleaseRateCalculator.performCalculation(requestForGas)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [!performRelease, _, hasReleaseRate, _] (rho is given)" in {

      baseRequestValues.setGasDensity(33.3).setMolarMass(0)

      val requestForGas = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(false)
        .setIsGasCalculation(true)
        .setHasReleaseRateInKgPerSecond(true)
        .setIsEvaporationFromPool(false)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val resultForGas = ReleaseRateCalculator.performCalculation(requestForGas)._1

      val requestForLiquid = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(false)
        .setIsGasCalculation(false)
        .setHasReleaseRateInKgPerSecond(true)
        .setIsEvaporationFromPool(false)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val resultForLiquid = ReleaseRateCalculator.performCalculation(requestForLiquid)._1

      resultForGas.calculate() should be(0.08091924259588931)
      resultForGas.texify() should be("""\dfrac{9.0}{33.3 \times 33.4 \times 0.1}""")

      resultForLiquid.calculate() should be(0.08091924259588931)
      resultForLiquid.texify() should be("""\dfrac{9.0}{33.3 \times 33.4 \times 0.1}""")
    }

    "return an expression [!performRelease, _, hasReleaseRate, _] (molar mass is given)" in {

      val requestForGas = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(false)
        .setIsGasCalculation(true)
        .setHasReleaseRateInKgPerSecond(true)
        .setIsEvaporationFromPool(false)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val resultForGas = ReleaseRateCalculator.performCalculation(requestForGas)._1

      val requestForLiquid = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(false)
        .setIsGasCalculation(false)
        .setHasReleaseRateInKgPerSecond(true)
        .setIsEvaporationFromPool(false)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val resultForLiquid = ReleaseRateCalculator.performCalculation(requestForLiquid)._1

      resultForGas.calculate() should be(0.6068943194691696)
      resultForGas.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")

      resultForLiquid.calculate() should be(0.6068943194691696)
      resultForLiquid.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [performRelease, !isGas, _, !pool] (third branch)" in {

      val request = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(true)
        .setIsGasCalculation(false)
        .setHasReleaseRateInKgPerSecond(false)
        .setIsEvaporationFromPool(false)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val result = ReleaseRateCalculator.performCalculation(request)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [performRelease, !isGas, _, pool] (fourth branch)" in {

      val request = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(true)
        .setIsGasCalculation(false)
        .setHasReleaseRateInKgPerSecond(false)
        .setIsEvaporationFromPool(true)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val result = ReleaseRateCalculator.performCalculation(request)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [performRelease, _, hasReleaseRate, _] (above critical gas pressure)" in {

      baseRequestValues.setCriticalGasPressure(22)

      val request = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(true)
        .setIsGasCalculation(true)
        .setHasReleaseRateInKgPerSecond(true)
        .setIsEvaporationFromPool(true)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val result = ReleaseRateCalculator.performCalculation(request)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [performRelease, _, hasReleaseRate, _] (below critical gas pressure)" in {
      val request = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(true)
        .setIsGasCalculation(true)
        .setHasReleaseRateInKgPerSecond(true)
        .setIsEvaporationFromPool(true)
        .setReleaseRateValues(baseRequestValues)
        .build()

      val result = ReleaseRateCalculator.performCalculation(request)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }
  }
}

