package com.sootsafe.server

import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass.ReleaseRateRequest
import org.scalatest.{Matchers, WordSpecLike}

class ReleaseRateCalculatorTest extends WordSpecLike with Matchers {

  "Calculator" must {
    "handle request" in {
      val request = ReleaseRateRequest
        .newBuilder()
        .setPerformReleaseCalculation(false)
        .setHasReleaseRateInKgPerSecond(false)
        .setIsGasCalculation(false)
        .build()

      ReleaseRateCalculator.handleRequest(request) should be(Right("Not yet implemented"))
    }


    "return an expression [!performRelease, hasReleaseRate, !isGas, !isEvaporation]" in {
      val result = ReleaseRateCalculator.performCalculation(performReleaseCalculation = false,
        hasReleaseRateInKgPerSecond = true,
        isGasCalculation = false,
        isEvaporationFromPool = false,
        .44, 33.4, 0.1, 9, None, None, 0, 0, 0, 0, 0, 0, 0, 0)

      result.calculate() should be(0.1317365269461078)
      result.texify() should be("""\dfrac{0.44}{33.4 \times 0.1}""")
    }

    "return an expression [!performRelease, hasReleaseRate, _, _] (rho is given)" in {
      val resultForGas = ReleaseRateCalculator.performCalculation(performReleaseCalculation = false,
        hasReleaseRateInKgPerSecond = true,
        isGasCalculation = true,
        isEvaporationFromPool = false,
        .44, 33.4, 0.1, 9, None, Some(33.3), 0, 0, 0, 0, 0, 0, 0, 0)

      val resultForLiquid = ReleaseRateCalculator.performCalculation(performReleaseCalculation = false,
        hasReleaseRateInKgPerSecond = true,
        isGasCalculation = true,
        isEvaporationFromPool = false,
        .44, 33.4, 0.1, 9, None, Some(33.3), 0, 0, 0, 0, 0, 0, 0, 0)

      resultForGas.calculate() should be(0.08091924259588931)
      resultForGas.texify() should be("""\dfrac{0.2702702702702703}{33.4 \times 0.1}""")

      resultForLiquid.calculate() should be(0.08091924259588931)
      resultForLiquid.texify() should be("""\dfrac{0.2702702702702703}{33.4 \times 0.1}""")
    }

    "return an expression [!performRelease, hasReleaseRate, _, _] (molar mass is given)" in {
      val resultForGas = ReleaseRateCalculator.performCalculation(performReleaseCalculation = false,
        hasReleaseRateInKgPerSecond = true,
        isGasCalculation = true,
        isEvaporationFromPool = false,
        .44, 33.4, 0.1, 9, Some(33.3), None, 0, 0, 0, 0, 0, 0, 0, 0)

      val resultForLiquid = ReleaseRateCalculator.performCalculation(performReleaseCalculation = false,
        hasReleaseRateInKgPerSecond = true,
        isGasCalculation = true,
        isEvaporationFromPool = false,
        .44, 33.4, 0.1, 9, Some(33.3), None, 0, 0, 0, 0, 0, 0, 0, 0)

      resultForGas.calculate() should be(2.609382631100853)
      resultForGas.texify() should be("""\dfrac{8.715337987876849}{33.4 \times 0.1}""")

      resultForLiquid.calculate() should be(2.609382631100853)
      resultForLiquid.texify() should be("""\dfrac{8.715337987876849}{33.4 \times 0.1}""")
    }

    "return an expression [performRelease, !hasReleaseRate, _, !pool] (molar mass is given)" in {

      val resultForLiquid = ReleaseRateCalculator.performCalculation(performReleaseCalculation = true,
        hasReleaseRateInKgPerSecond = false,
        isGasCalculation = true,
        isEvaporationFromPool = false,
        .44, 33.4, 0.1, 9, Some(33.3), None, 7, 87, 0.7, 0, 0, 0, 0, 0)

      resultForLiquid.calculate() should be(228.85857597760383)
      resultForLiquid.texify() should be("""\dfrac{764.3876437651968}{33.4 \times 0.1}""")
    }

    "return an expression [performRelease, !hasReleaseRate, _, pool] (molar mass is given)" in {

      val resultForLiquid = ReleaseRateCalculator.performCalculation(performReleaseCalculation = true,
        hasReleaseRateInKgPerSecond = false,
        isGasCalculation = true,
        isEvaporationFromPool = true,
        .44, 33.4, 0.1, 9, Some(33.3), None, 7, 87, 0.7, 6, 6, 6, 6, 6)

      resultForLiquid.calculate() should be(79.60948910803178)
      resultForLiquid.texify() should be("""\dfrac{265.89569362082614}{33.4 \times 0.1}""")
    }
  }

}
