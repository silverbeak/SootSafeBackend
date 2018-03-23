package com.sootsafe.engine.zone

import com.sootsafe.arithmetic._
import com.sootsafe.reporting._
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass._

import scala.util.{Failure, Success, Try}

object ReleaseRateCalculator extends Symbols with RequestUtils {

  private implicit val reportFormat: ReportFormat = DefaultReportFormat

  def handleRequest(request: ReleaseRateRequest): Either[ReleaseRateCalculationResult, String] = {

    Try(performCalculation(request)) match {
      case Success(releaseRateExpression) =>
        val entry = ReleaseRateResultEntry
          .newBuilder()
          .setKey(request.getKey)
          .setReleaseCharacter(releaseRateExpression.calculate())
          .build()

        val zoneExtent = ZoneCalculator.determinePollutionDistance(request.getReleaseType, releaseRateExpression)

        val zoneExtentSection = ReleaseRateReportGenerator.generateCalculationChapter(CalculationChapter(Seq(zoneExtent)))

//        println(s"ZoneExtent: \n$zoneExtentSection")

        val zoneLabel = ZoneCalculator.calculateZoneExtent(request, releaseRateExpression)

        val result = ReleaseRateCalculationResult
          .newBuilder()
          .setReleaseRateResult(entry)
          //          .setZoneExtent(zoneExtent.expression.calculate())
          .setZoneLabel(zoneLabel)
          .build()

        Left(result)

      case Failure(e) =>
        Right(s"Could not calculate release rate character. Error: ${e.getMessage}")
    }
  }

  def performCalculation(request: ReleaseRateRequest): Expression = {

    val performReleaseCalculation = request.getPerformReleaseCalculation
    val isGasCalculation = request.getIsGasCalculation
    val hasReleaseRateInKgPerSecond = request.getHasReleaseRateInKgPerSecond
    val isEvaporationFromPool = request.getIsEvaporationFromPool


    val values = request.getReleaseRateValues

    val Qg = getValue(values.getVolumetricGasFlowRate)
    val k = getValue(values.getSafetyFactor)
    val lfl = getValue(values.getLowerFlammableLimit)
    val Wg = getValue(values.getMassReleaseRate)
    val M = getValue(values.getMolarMass)
    val rhoG = getValue(values.getGasDensity)
    val Cd = getValue(values.getDischargeCoefficient)
    val S = getValue(values.getCrossSectionArea)
    val deltaP = getValue(values.getPressureDifference)
    val Ap = getValue(values.getPoolSurfaceArea)
    val uw = getValue(values.getWindSpeed)
    val T = getValue(values.getAbsoluteTemperature)
    val gma = getValue(values.getAdiabaticExpansion)
    val pa = getValue(values.getAtmosphericPressure)
    val criticalGasPressure = getValue(values.getCriticalGasPressure)
    val compressibilityFactor = getValue(values.getCompressibilityFactor)

    val calculationSequence = prepareSymbols(performReleaseCalculation,
      isGasCalculation,
      hasReleaseRateInKgPerSecond,
      isEvaporationFromPool,
      Qg, Wg, M, rhoG, Cd, S, deltaP, Ap, uw, T, gma, pa,
      criticalGasPressure,
      compressibilityFactor)

    val kSymbol = Symbol(k, "k")
    val lflSymbol = Symbol(lfl, "LFL")
    val calculatedQq = calculationSequence.last
    val qgSymbol = Symbol(Value(calculatedQq.formula.calculate()), "Q_g")
    val WgSymbol = Symbol(Wg, "W_g")
    val rhoGSymbol = Symbol(rhoG, """\\rho_G""")

    val formula = new ReleaseCharacter2(WgSymbol, rhoGSymbol, kSymbol, lflSymbol)

    //    val latex = ReleaseRateReportGenerator.generateLatex(calculationSequence :+ FormulaContainer(formula, Some("Release characteristics")))

    //    println(s"Texified:\n$latex")

    formula
  }

  case class FormulaContainer(formula: Formula, description: Option[String] = None, decision: Option[String] = None)

  private[zone] def prepareSymbols(performReleaseCalculation: Boolean,
                                   isGasCalculation: Boolean,
                                   hasReleaseRateInKgPerSecond: Boolean,
                                   isEvaporationFromPool: Boolean,
                                   Qg: Expression,
                                   Wg: Expression,
                                   M: Expression,
                                   rhoG: Expression,
                                   Cd: Expression,
                                   S: Expression,
                                   deltaP: Expression,
                                   Ap: Expression,
                                   uw: Expression,
                                   T: Expression,
                                   gma: Expression,
                                   pa: Expression,
                                   criticalPressure: Expression,
                                   compressibilityFactor: Expression): Seq[FormulaContainer] = {
    val R = 8324d
    val rSymbol = Symbol(Value(R), "R")

    (performReleaseCalculation, isGasCalculation, hasReleaseRateInKgPerSecond, isEvaporationFromPool) match {
      case (false, true, false, _) =>
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        Seq(FormulaContainer(new PlainFormula(Qg), None))

      case (false, false, false, _) =>
        // För vätska: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        Seq(FormulaContainer(new PlainFormula(Qg), None))

      case (false, _, true, _) =>
        // För gas: Beräkna Qg(ekv B.5)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        val b5formula = calculateB5(Wg, M, rhoG)
        Seq(FormulaContainer(b5formula, None))

      case (true, false, _, false) =>
        // För gas: Beräkna Wg(ekv B.1)
        // För gas: Beräkna Qg(ekv B.5)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)

        val cdSymbol = Symbol(Cd, "C_d")
        val sSymbol = Symbol(S, "S")
        val deltaPSymbol = Symbol(deltaP, s"${delta.sign}p")

        val wg = new ReleaseRateOfLiquid(cdSymbol, sSymbol, deltaPSymbol)

        val b5formula = calculateB5(wg, M, rhoG)
        Seq(FormulaContainer(wg, None), FormulaContainer(b5formula, None))


      case (true, false, _, true) =>
        // För gas: Beräkna Wg(ekv B.6)
        // För gas: Beräkna Qg(ekv B.7)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)

        val uwSymbol = Symbol(uw, "u_w")
        val apSymbol = Symbol(Ap, "A_p")
        val pvSymbol = Symbol(pa, "p")
        val mSymbol = Symbol(M, "M")
        val tSymbol = Symbol(T, "T")

        val we = new Evaporation(uwSymbol, apSymbol, pvSymbol, mSymbol, rSymbol, tSymbol)

        val b5formula = calculateB5(we, M, rhoG)
        Seq(FormulaContainer(we, None), FormulaContainer(b5formula, None))

      case (true, true, _, _) =>
        // För vätska: Beräkna kritiskt tryck  (ekv B.2)

        val gmaSymbol = gamma.copy(expression = gma)
        val paSymbol = Symbol(pa, "p_a")
        val mSymbol = Symbol(M, "M")
        val tSymbol = Symbol(T, "T")
        val cdSymbol = Symbol(Cd, "C_d")
        val sSymbol = Symbol(S, "S")
        val zSymbol = Symbol(compressibilityFactor, "Z")
        val pSymbol = Symbol(deltaP, "p")

        val gasPressure = new CriticalGasPressure(paSymbol, gmaSymbol)

        val wg = if (gasPressure > criticalPressure) {
          // För vätska: Beräkna Wg genom B.3
          new NonLimitedGasRate(cdSymbol, sSymbol, mSymbol, rSymbol, tSymbol, zSymbol, gmaSymbol, paSymbol, pSymbol)
        } else {
          // För vätska: Beräkna Wg genom B.4
          new LimitedGasRate(cdSymbol, sSymbol, mSymbol, rSymbol, tSymbol, zSymbol, gmaSymbol, pSymbol)
        }

        val b5formula = calculateB5(wg, M, rhoG)
        Seq(FormulaContainer(wg, Some("Mass release rate of gas")), FormulaContainer(b5formula, Some("Volumetric gas flow")))
    }
  }

  private[zone] def calculateB5(wg: Expression, M: Expression, rhoG: Expression): Formula = {

    val rhoSymbol = (M, rhoG) match {
      case (Expression.Zero, rho_g) =>
        rho.copy(expression = rho_g)

      case (m, Expression.Zero) =>
        // First, calculate rho based on M
        val mSymbol = Symbol(m, "M")
        val paSymbol = Symbol(Value(101325), "p_a")
        val RSymbol = Symbol(Value(8314), "R")
        val TaSymbol = Symbol(Value(393), "T_a")
        val calculatedRho = new MolarMassToRho(mSymbol, paSymbol, RSymbol, TaSymbol).calculate()

        rho.copy(expression = Value(calculatedRho))

      case (_, rho_g) =>
        rho.copy(expression = rho_g)


      case _ => throw new Exception("Rho or M must be specified")
    }

    val wgSymbol = Symbol(wg.toValue, "W_g")
    new VolumetricGasFlow(wgSymbol, rhoSymbol)
  }

}