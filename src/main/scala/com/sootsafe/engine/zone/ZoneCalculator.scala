package com.sootsafe.engine.zone

import com.sootsafe.arithmetic._
import com.sootsafe.server.{Element, ElementTable}
import AtexCalculator.{FormulaContainer, getValue}
import com.sootsafe.reporting._
import com.sootsafe.reporting.figures.{FigureC1, FigureD1}
import com.sootsafe.reporting.tables.{TableC1, TableD1}
import com.sootsafe.server.calculator.AtexCalculator._


object ZoneCalculator {
  def determineZone(gradeOfRelease: GradeOfRelease,
                    dilutionLevel: DilutionLevel.Value,
                    ventilationAvailability: VentilationAvailability): FormulaSection = {
    val result = (gradeOfRelease, dilutionLevel, ventilationAvailability) match {
      case (GradeOfRelease.Continuous, DilutionLevel.High, VentilationAvailability.Good) => "Non-hazardous (Zone 0 NE)"
      case (GradeOfRelease.Primary, DilutionLevel.High, VentilationAvailability.Good) => "Non-hazardous (Zone 1 NE)"
      case (GradeOfRelease.Secondary, DilutionLevel.High, VentilationAvailability.Good) => "Non-hazardous (Zone 2 NE)"

      case (GradeOfRelease.Continuous, DilutionLevel.High, VentilationAvailability.Fair) => "Zone 2 (Zone 0 NE)"
      case (GradeOfRelease.Primary, DilutionLevel.High, VentilationAvailability.Fair) => "Zone 2 (Zone 1 NE)"
      case (GradeOfRelease.Secondary, DilutionLevel.High, VentilationAvailability.Fair) => "Non-hazardous (Zone 2 NE)"

      case (GradeOfRelease.Continuous, DilutionLevel.High, VentilationAvailability.Poor) => "Zone 1 (Zone 0 NE)"
      case (GradeOfRelease.Primary, DilutionLevel.High, VentilationAvailability.Poor) => "Zone 2 (Zone 1 NE)"
      case (GradeOfRelease.Secondary, DilutionLevel.High, VentilationAvailability.Poor) => "Zone 2"

      case (GradeOfRelease.Continuous, DilutionLevel.Medium, VentilationAvailability.Good) => "Zone 0"
      case (GradeOfRelease.Primary, DilutionLevel.Medium, VentilationAvailability.Good) => "Zone 1"
      case (GradeOfRelease.Secondary, DilutionLevel.Medium, VentilationAvailability.Good) => "Zone 2"

      case (GradeOfRelease.Continuous, DilutionLevel.Medium, VentilationAvailability.Fair) => "Zone 0 + Zone 2"
      case (GradeOfRelease.Primary, DilutionLevel.Medium, VentilationAvailability.Fair) => "Zone 1 + Zone 2"
      case (GradeOfRelease.Secondary, DilutionLevel.Medium, VentilationAvailability.Fair) => "Zone 2"

      case (GradeOfRelease.Continuous, DilutionLevel.Medium, VentilationAvailability.Poor) => "Zone 0 + Zone 1"
      case (GradeOfRelease.Primary, DilutionLevel.Medium, VentilationAvailability.Poor) => "Zone 1 + Zone 2"
      case (GradeOfRelease.Secondary, DilutionLevel.Medium, VentilationAvailability.Poor) => "Zone 2"

      case (GradeOfRelease.Continuous, DilutionLevel.Low, _) => "Zone 0"
      case (GradeOfRelease.Primary, DilutionLevel.Low, _) => "Zone 1 or Zone 0"
      case (GradeOfRelease.Secondary, DilutionLevel.Low, _) => "Zone 1 and even Zone 0"

      case x => throw new Exception(s"Could not determine zone from combination [$x]")
    }

    FormulaSection(
      None,
      Some(Decision(s"""$gradeOfRelease release grade, $dilutionLevel dilution level and $ventilationAvailability ventilation availability gives $result based on Table \\ref{table:${TableD1.identifier}}""")),
      Some(Description("Determining the zone type")),
      Seq(TableD1)
    )
  }

  private[zone] def calculateZoneExtent(request: AtexRequest, releaseCharacter: Expression): Seq[FormulaSection] = {

    ElementTable.elements.get(request.casNumber) match {
      case None => ???
      case Some(element) =>
        //val lfl = getValue(request.getReleaseRateValues.getLowerFlammableLimit)

        val backgroundConcentration = if (request.isIndoors) Some(determineBackgroundConcentration(request))
        else None

        val backgroundConcentrationSection = backgroundConcentration match {
          case Some(bgConcentration) =>
            FormulaSection(
              Some(FormulaContainer(bgConcentration)),
              None,
              Some(Description("Background Concentration"))
            )
          case None =>
            FormulaSection(
              None,
              Some(Decision("Background concentration is not determined because leakage is outdoors")),
              Some(Description("Background Concentration"))
            )
        }

        val ventilationVelocity = determineVentilationVelocity(request, element)
        val dilutionLevel = determineDilutionLevel(backgroundConcentration, ventilationVelocity._2, Value(request.getReleaseRate.lowerFlammableLimit), releaseCharacter, request.isIndoors)
        val zoneSection = ZoneCalculator.determineZone(request.gradeOfRelease, dilutionLevel._2, request.ventilationAvailability)
        Seq(backgroundConcentrationSection, ventilationVelocity._1, dilutionLevel._1, zoneSection)
    }
  }

  private[zone] def determineBackgroundConcentration(request: AtexRequest): Formula = {

    val backgroundConcentrationValues = request.getBackgroundConcentration

    val QgSymbol = Symbol(getValue(request.getReleaseRate.volumetricGasFlowRate), "Q_g")
    val fSymbol = Symbol(getValue(backgroundConcentrationValues.ventilationEfficiencyFactor), "f")
    val Q1Symbol = Symbol(getValue(backgroundConcentrationValues.volumetricFlowAir), "Q_1")
    //    val Q2Symbol = Symbol(getValue(backgroundConcentrationValues.volumetricFlowAirGas), "Q_2")
    val QaSymbol = Symbol(getValue(backgroundConcentrationValues.airEnteringRoomFlowRate), "Q_A")
    val CSymbol = Symbol(getValue(backgroundConcentrationValues.airChangeFrequency), "C")
    val roomLSymbol = Symbol(getValue(backgroundConcentrationValues.roomDimensions.get.depth), "L")
    val roomBSymbol = Symbol(getValue(backgroundConcentrationValues.roomDimensions.get.width), "W")
    val roomHSymbol = Symbol(getValue(backgroundConcentrationValues.roomDimensions.get.height), "H")
    val V0Symbol = Symbol(roomLSymbol.expression * roomHSymbol.expression * roomBSymbol.expression, "V_0")
    val SSymbol = Symbol(getValue(backgroundConcentrationValues.crossSectionArea), "S")

    val Q2Symbol = Symbol(CSymbol.expression * V0Symbol.expression, "Q_2")

    if (Q2Symbol.expression.toValue == Expression.Zero) {
      new BackgroundConcentrationFormulaV1(fSymbol, QgSymbol, QaSymbol)
    } else {
      new BackgroundConcentrationFormulaV2(fSymbol, QgSymbol, Q2Symbol)
    }
  }

  private[zone] def determineVentilationVelocity(request: AtexRequest, element: Element): (FormulaSection, Expression) = {
    val heavierThanAir = element.RDT > 1
    if (request.isIndoors) {
      val roomLSymbol = Symbol(getValue(request.getBackgroundConcentration.getRoomDimensions.depth), "L")
      val roomHSymbol = Symbol(getValue(request.getBackgroundConcentration.getRoomDimensions.height), "H")
      val airFlow = Symbol(getValue(request.getBackgroundConcentration.airEnteringRoomFlowRate), "Q_A")

      val ventilationVelocity = new VentilationVelocityFormula(airFlow, roomHSymbol, roomLSymbol)

      val ventilationSection = FormulaSection(
        Some(FormulaContainer(ventilationVelocity)),
        Some(Decision("Since the leakage is indoors, the ventilation velocity is calculated based on the room volume and the air flow.")),
        Some(Description("Total ventilation velocity"))
      )

      (ventilationSection, ventilationVelocity)
    } else {
      // This is all determined from table C.1
      val (description, ventilationVelocity) = (request.isEvaporationFromPool, request.getVentilationVelocityValues.obstructed, heavierThanAir, request.getVentilationVelocityValues.elevation) match {
        case (true, Obstruction.Unobstructed, _, _) => ("> 0.25 m/s", Value(0.25))
        case (true, Obstruction.Obstructed, _, _) => ("> 0.1 m/s", Value(0.1))

        case (_, Obstruction.Unobstructed, false, d) if d <= 2d => ("0.5 m/s", Value(0.5))
        case (_, Obstruction.Unobstructed, false, d) if d <= 5d => ("2 m/s", Value(2))
        case (_, Obstruction.Unobstructed, false, _) => ("1 m/s", Value(1))

        case (_, Obstruction.Obstructed, false, d) if d <= 2d => ("0.5 m/s", Value(0.5))
        case (_, Obstruction.Obstructed, false, d) if d <= 5d => ("1 m/s", Value(1))
        case (_, Obstruction.Obstructed, false, _) => ("0.5 m/s", Value(0.5))

        case (_, Obstruction.Unobstructed, true, d) if d <= 2d => ("0.3 m/s", Value(0.3))
        case (_, Obstruction.Unobstructed, true, d) if d <= 5d => ("1 m/s", Value(1))
        case (_, Obstruction.Unobstructed, true, _) => ("0.6 m/s", Value(0.6))

        case (_, Obstruction.Obstructed, true, d) if d <= 2d => ("0.15 m/s", Value(0.15))
        case (_, Obstruction.Obstructed, true, d) if d <= 5d => ("1 m/s", Value(1))
        case (_, Obstruction.Obstructed, true, _) => ("0.3 m/s", Value(0.3))
      }

      val ventilationSection = FormulaSection(
        None,
        Some(Decision(s"Ventilation velocity for outdoor leakage is determined to be $description from table \\ref{table:${TableC1.identifier}}")),
        Some(Description(s"Total ventilation velocity")),
        Seq(TableC1)
      )

      (ventilationSection, ventilationVelocity)
    }
  }

  private def ykxm(k: Expression = Value(1), m: Expression = Expression.Zero)(x: Expression): Expression = k * x + m

  private def ykxmLog(constant: Expression = Value(1), m: Expression = Value(1))(x: Expression): Expression = (x ^ m) * constant

  private[zone] def determinePollutionDistance(releaseType: ReleaseType, releaseCharacter: Expression): FormulaSection = {

    def releaseTypeToLineFunction(releaseType: ReleaseType): Expression => Expression = releaseType match {
      // The constant (offset) values for these functions have been determined by back tracing the graphs in figure D.1 the document
      case ReleaseType.HeavyGas => ykxmLog(constant = Value(9), m = Value(0.5))(_)
      case ReleaseType.DiffusiveJet => ykxmLog(constant = Value(4.2), m = Value(0.5))(_)
      case ReleaseType.Jet => ykxmLog(constant = Value(1.8), m = Value(0.5))(_)
      case x => throw new Exception(s"Release type $x not recognized")
    }

    val line = releaseTypeToLineFunction(releaseType)

    val pollutionDistanceFormula = new PlainFormula(line(releaseCharacter))
    FormulaSection(
      Some(FormulaContainer(pollutionDistanceFormula)),
      Some(Decision(s"Release type $releaseType and release character = ${releaseCharacter.toValue.texify()}$$(m^3/s)$$ gives a pollution radius of ${pollutionDistanceFormula.toValue.texify()}$$(m)$$, based on Diagram \\ref{fig:${FigureD1.identifier}}")),
      Some(Description("Determine pollution radius")),
      Nil,
      Seq(FigureD1)
    )
  }

  private[zone] def determineDilutionLevel(backgroundConcentration: Option[Formula], ventilationVelocity: Expression, lfl: Expression, releaseCharacter: Expression, isIndoors: Boolean): (FormulaSection, DilutionLevel.Value) = {
    // Figure C.1
    (isIndoors, backgroundConcentration.getOrElse(new PlainFormula(Expression.Zero)).calculate() > 0.25 * lfl.calculate()) match {
      case (false, true) =>

        val dilutionLevelSection = FormulaSection(
          None,
          Some(Decision("The dilution level is determined to be low, since the leakage is outdoors, and the background concentration is below 25% of the lower flammability level")),
          Some(Description("Determine dilution level"))
        )

        (dilutionLevelSection, DilutionLevel.Low)
      case _ =>
        val lowLimit = ykxmLog(m = Value(1), constant = Value(0.045))(_)
        val highLimit = ykxmLog(m = Value(1), constant = Value(13.5))(_)

        val dilutionLevel = ventilationVelocity.calculate() match {
          case ventVelocity if ventVelocity < lowLimit(releaseCharacter).calculate() => DilutionLevel.Low
          case ventVelocity if ventVelocity > highLimit(releaseCharacter).calculate() => DilutionLevel.High
          case _ => DilutionLevel.Medium
        }

        val dilutionLevelSection = FormulaSection(
          None,
          Some(Decision(s"The dilution level has been determined to be $dilutionLevel based on Diagram \\ref{fig:${FigureC1.identifier}}")),
          Some(Description("Determine dilution level")),
          Nil,
          Seq(FigureC1)
        )

        (dilutionLevelSection, dilutionLevel)
    }
  }
}

object ZoneType extends Enumeration {
  val Zone0: Value = Value
  val Zone1: Value = Value
  val Zone2: Value = Value
}


object DilutionLevel extends Enumeration {
  val Low: Value = Value
  val Medium: Value = Value
  val High: Value = Value
}
