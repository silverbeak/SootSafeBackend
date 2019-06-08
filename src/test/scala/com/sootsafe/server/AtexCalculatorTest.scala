package com.sootsafe.server

import com.sootsafe.engine.zone.AtexCalculator
import com.sootsafe.server.calculator.AtexCalculator.{AtexFields, AtexRequest, ReleaseRateValues}
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpecLike}

class AtexCalculatorTest extends WordSpecLike with Matchers with BeforeAndAfterEach {

  private var baseRequestValues: ReleaseRateValues = _

  override def beforeEach(): Unit = {
    baseRequestValues = ReleaseRateValues(
      volumetricGasFlowRate = .44,
      safetyFactor = 33.4,
      lowerFlammableLimit = .1,
      evaporationRate = 9,
      molarMass = 33.3,
      gasDensity = 4.44,
      dischargeCoefficient = 7,
      pressureDifference = 44,
      poolSurfaceArea = 3,
      windSpeed = 3,
      absoluteTemperature = 3,
      adiabaticExpansion = 55,
      atmosphericPressure = 33,
      criticalGasPressure = 1000,
      compressibilityFactor = 11
    )


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
      val request = AtexRequest(
        Some(AtexFields(
          releaseRate = Some(baseRequestValues),
          casNumber = "74-86-2"
        ))
      )

      AtexCalculator.handleRequest(request) match {
        case Right(errorString) => fail(errorString)
        case Left(result) =>
          result.getAtexResult.key should be(request.fields.get.key)
          result.getAtexResult.releaseCharacter should be(0.6068943194691696)
      }
    }

    //    "handle request based on json" in {
    //      val builder = AtexCalculatorOuterClass.AtexRequest.newBuilder
    //      val request = MessageSerializer.serializer[AtexRequest](FakeMessage.jsonMsg, builder)
    //
    //      AtexCalculator.handleRequest(request) match {
    //        case Right(errorString) => fail(errorString)
    //        case Left(result) =>
    //          result.getAtexResult.getKey should be(request.getKey)
    //          result.getAtexResult.getReleaseCharacter should be(0.1111111111111111)
    //      }
    //    }

    "return an expression [!performRelease, !isGas, hasReleaseRate, !isEvaporation]" in {

      val requestForGas = AtexFields(
        performReleaseCalculation = false,
        isGasCalculation = false,
        hasReleaseRateInKgPerSecond = false,
        isEvaporationFromPool = false,
        releaseRate = Some(baseRequestValues)
      )


      val result = AtexCalculator.performCalculation(requestForGas)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [!performRelease, _, hasReleaseRate, _] (rho is given)" in {

      val requestForGas = AtexFields(
        performReleaseCalculation = false,
        isGasCalculation = true,
        hasReleaseRateInKgPerSecond = true,
        isEvaporationFromPool = false,
        releaseRate = Some(baseRequestValues.copy(gasDensity = 33.3, molarMass = 0))
        //        releaseRate = Some(baseRequestValues)
      )

      val resultForGas = AtexCalculator.performCalculation(requestForGas)._1

      val requestForLiquid = AtexFields(
        performReleaseCalculation = false,
        isGasCalculation = false,
        hasReleaseRateInKgPerSecond = true,
        isEvaporationFromPool = false,
        releaseRate = Some(baseRequestValues.copy(gasDensity = 33.3, molarMass = 0))
      )


      val resultForLiquid = AtexCalculator.performCalculation(requestForLiquid)._1

      resultForGas.calculate() should be(0.08091924259588931)
      resultForGas.texify() should be("""\dfrac{9.0}{33.3 \times 33.4 \times 0.1}""")

      resultForLiquid.calculate() should be(0.08091924259588931)
      resultForLiquid.texify() should be("""\dfrac{9.0}{33.3 \times 33.4 \times 0.1}""")
    }

    "return an expression [!performRelease, _, hasReleaseRate, _] (molar mass is given)" in {

      val requestForGas = AtexFields(
        performReleaseCalculation = false,
        isGasCalculation = true,
        hasReleaseRateInKgPerSecond = true,
        isEvaporationFromPool = false,
        releaseRate = Some(baseRequestValues)
      )

      val resultForGas = AtexCalculator.performCalculation(requestForGas)._1

      val requestForLiquid = AtexFields(
        performReleaseCalculation = false,
        isGasCalculation = false,
        hasReleaseRateInKgPerSecond = true,
        isEvaporationFromPool = false,
        releaseRate = Some(baseRequestValues)
      )

      val resultForLiquid = AtexCalculator.performCalculation(requestForLiquid)._1

      resultForGas.calculate() should be(0.6068943194691696)
      resultForGas.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")

      resultForLiquid.calculate() should be(0.6068943194691696)
      resultForLiquid.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [performRelease, !isGas, _, !pool] (third branch)" in {

      val request = AtexFields(
        performReleaseCalculation = true,
        isGasCalculation = false,
        hasReleaseRateInKgPerSecond = false,
        isEvaporationFromPool = false,
        releaseRate = Some(baseRequestValues)
      )

      val result = AtexCalculator.performCalculation(request)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [performRelease, !isGas, _, pool] (fourth branch)" in {

      val request = AtexFields(
        performReleaseCalculation = true,
        isGasCalculation = false,
        hasReleaseRateInKgPerSecond = false,
        isEvaporationFromPool = true,
        releaseRate = Some(baseRequestValues)
      )

      val result = AtexCalculator.performCalculation(request)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [performRelease, _, hasReleaseRate, _] (above critical gas pressure)" in {
      val request = AtexFields(
        performReleaseCalculation = true,
        isGasCalculation = true,
        hasReleaseRateInKgPerSecond = true,
        isEvaporationFromPool = true,
        releaseRate = Some(baseRequestValues.copy(criticalGasPressure = 22))
      )

      val result = AtexCalculator.performCalculation(request)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }

    "return an expression [performRelease, _, hasReleaseRate, _] (below critical gas pressure)" in {
      val request = AtexFields(
        performReleaseCalculation = true,
        isGasCalculation = true,
        hasReleaseRateInKgPerSecond = true,
        isEvaporationFromPool = true,
        releaseRate = Some(baseRequestValues)
      )

      val result = AtexCalculator.performCalculation(request)._1

      result.calculate() should be(0.6068943194691696)
      result.texify() should be("""\dfrac{9.0}{4.44 \times 33.4 \times 0.1}""")
    }
  }
}

