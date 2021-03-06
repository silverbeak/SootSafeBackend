package com.sootsafe.engine.zone

import com.google.protobuf.ByteString
import com.sootsafe.arithmetic._
import com.sootsafe.reporting._
import com.sootsafe.server.calculator.AtexCalculator.{AtexCalculationResult, AtexFields, AtexRequest, AtexResultEntry}

import scala.util.{Failure, Success, Try}

object AtexCalculator extends Symbols with RequestUtils {

  private implicit val reportFormat: ReportFormat = DefaultReportFormat

  def handleRequest(request: AtexRequest): Either[AtexCalculationResult, String] = {

    val atexFields = request.fields.get
    Try(performCalculation(atexFields)) match {
      case Success(releaseRateExpression) =>
        val entry = new AtexResultEntry(
          key = atexFields.key,
          releaseCharacter = releaseRateExpression._1.calculate()

        )

        val zoneExtent = ZoneCalculator.determinePollutionDistance(atexFields.releaseType, releaseRateExpression._1)

        val zoneFormulaSections = ZoneCalculator.calculateZoneExtent(atexFields, releaseRateExpression._1)

        val releaseRateCalculationSection = CalculationSection(
          Some(Description("Determine the release rate")),
          releaseRateExpression._2
        )

        val zoneCalculationSection = CalculationSection(
          Some(Description("Determine the zone type")),
          Seq(zoneExtent) ++ zoneFormulaSections
        )

        val authorName = Option(atexFields.getAtexMetadata.authorName)
        Try(AtexReportGenerator.generateLatex(Seq(releaseRateCalculationSection, zoneCalculationSection), authorName)) match {
          case Failure(e) =>
            Right(s"Error while generating Latex report. ${e.getClass.getName}:\n${e.getStackTrace.mkString("\n")}")
          case Success(zoneExtentReport) =>
            val result = new AtexCalculationResult(
              atexResult = Some(entry),
              latex = ByteString.copyFromUtf8(zoneExtentReport)
            )
            Left(result)
        }

      case Failure(e) =>
        Right(s"Could not calculate release rate character. Error: ${e.getMessage}")
    }
  }

  def performCalculation(request: AtexFields): (Expression, Seq[FormulaSection]) = {

    val performReleaseCalculation = request.performReleaseCalculation
    val isGasCalculation = request.isGasCalculation
    val hasReleaseRateInKgPerSecond = request.hasReleaseRateInKgPerSecond
    val isEvaporationFromPool = request.isEvaporationFromPool


    val values = request.getReleaseRate

    val Qg = getValue(values.volumetricGasFlowRate)
    val k = getValue(values.safetyFactor)
    val lfl = getValue(values.lowerFlammableLimit)
    val We = getValue(values.evaporationRate)
    val M = getValue(values.molarMass)
    val rhoG = getValue(values.gasDensity)
    val Cd = getValue(values.dischargeCoefficient)
    val S = getValue(request.getBackgroundConcentration.crossSectionArea)
    val deltaP = getValue(values.pressureDifference)
    val Ap = getValue(values.poolSurfaceArea)
    val uw = getValue(values.windSpeed)
    val T = getValue(values.absoluteTemperature)
    val gma = getValue(values.adiabaticExpansion)
    val pa = getValue(values.atmosphericPressure)
    val criticalGasPressure = getValue(values.criticalGasPressure)
    val compressibilityFactor = getValue(values.compressibilityFactor)

    val calculationSequence = prepareSymbols(performReleaseCalculation,
      isGasCalculation,
      hasReleaseRateInKgPerSecond,
      isEvaporationFromPool,
      Qg, We, M, rhoG, Cd, S, deltaP, Ap, uw, T, gma, pa,
      criticalGasPressure,
      compressibilityFactor)

    val kSymbol = Symbol(k, "k")
    val lflSymbol = Symbol(lfl, "LFL")
    //    val calculatedQq = calculationSequence.last
    //    val qgSymbol = Symbol(Value(calculatedQq.formula.calculate()), "Q_g")
    val WgSymbol = Symbol(We, "W_e")
    val rhoGSymbol = Symbol(rhoG, """\rho_G""")

    val formula = new ReleaseCharacter2(WgSymbol, rhoGSymbol, kSymbol, lflSymbol)

    val formulaCollection = calculationSequence :+ FormulaContainer(formula, Some("Release characteristics"))

    (formula, formulaCollection.map(container => FormulaSection(
      Option(container),
      container.decision.map(Decision),
      container.description.map(Description)))
    )
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
        Seq(FormulaContainer(new PlainFormula(Qg), Some("Characteristic of release")))

      case (false, false, false, _) =>
        // För vätska: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        Seq(FormulaContainer(new PlainFormula(Qg), Some("Characteristic of release")))

      case (false, _, true, _) =>
        // För gas: Beräkna Qg(ekv B.5)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        //        val b5formula = calculateB5(Wg, M, rhoG)
        //        Seq(FormulaContainer(b5formula, Some("Volumetric flow rate")))
        Nil

      case (true, false, _, false) =>
        // För gas: Beräkna Wg(ekv B.1)
        // För gas: Beräkna Qg(ekv B.5)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)

        val cdSymbol = Symbol(Cd, "C_d")
        val sSymbol = Symbol(S, "S")
        val deltaPSymbol = Symbol(deltaP, s"${Delta.sign}p")

        val wg = new ReleaseRateOfLiquid(cdSymbol, sSymbol, deltaPSymbol)

        val b5formula = calculateB5(wg, M, rhoG)
        Seq(FormulaContainer(wg, Some("Release rate")), FormulaContainer(b5formula, Some("Volumetric flow rate")))


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
        Seq(FormulaContainer(we, Some("Evaporation rate")), FormulaContainer(b5formula, Some("Volumetric flow rate")))

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