package com.sootsafe.arithmetic

import java.util.UUID

import com.sootsafe.model.NodeModule
import com.sootsafe.reporting.Fixture.Latex

trait FormulaParameter

trait Formula extends Expression {

  val reference: Option[String]

  def texifyFormula(includeUnit: Boolean = false): String

  def getExpression: Expression

  def identifier: UUID
  
  def unit: String
}

object DynamicPressure {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = None
}

class DynamicPressure(nodeModule: NodeModule) extends Formula with Symbols {

  private val capacity = Value(nodeModule.ssInfo.capacity.get / 1000)

  private val diameter = Value(nodeModule.ssInfo.dimension.diameter.get / 1000)

  override def texifyFormula(includeUnit: Boolean = false): String = """\zeta = \dfrac{ \pi^{2}d^{4} \Delta p_j } { 8 \rho q^{2} }"""

  override def texify(): String = getExpression.texify()

  def getExpression: Expression = {
    val numerator = Value(8) * rho.expression * (capacity ^ Value(2))
    val denominator = (pi.expression ^ Value(2)) * (diameter ^ Value(4))
    numerator / denominator
  }

  override def calculate(): Double = getExpression.calculate()

  override val reference: Option[String] = DynamicPressure.reference

  override def identifier: UUID = DynamicPressure.identifier

  override def unit: String = ???
}

object Evaporation {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.6: Områden med explosiv gasatmosfär")
}

class Evaporation(uw: Symbol, ap: Symbol, pv: Symbol, M: Symbol, R: Symbol, T: Symbol) extends Formula with Symbols with Units {

  private val we = Symbol(Expression.Zero, "W_e")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${we.sign} = \\dfrac{6,55\\ ${uw.sign}^{0,78}\\ ${ap.sign}\\ ${pv.sign}\\ ${M.sign}^{0,667}}{${R.sign}\\ ${T.sign}}\\ $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    val numerator = Value(6.55) * (uw.expression ^ Value(0.78)) * ap.expression * pv.expression * (M.expression ^ Value(0.667))
    val denominator = R.expression * T.expression
    numerator / denominator
  }

  override val reference: Option[String] = Evaporation.reference

  override def identifier: UUID = Evaporation.identifier

  override def unit: String = s"($kg_per_second)"
}

object VolumetricEvaporation {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.7: Områden med explosiv gasatmosfär")
}

class VolumetricEvaporation(uw: Symbol, ap: Symbol, pv: Symbol, M: Symbol, T: Symbol, Ta: Symbol) extends Formula with Symbols with Units {

  private val Qg = Symbol(Expression.Zero, "Q_g")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${Qg.sign} \\approx \\dfrac{6,5\\ ${uw.sign}^{0,78}\\ ${ap.sign}\\ ${pv.sign}}{10^5\\ ${M.sign}^{0,333}} \\times \\dfrac {${Ta.sign}}{${T.sign}}\\ $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    val numerator1 = Value(6.5) * (uw.expression ^ Value(0.78)) * ap.expression * pv.expression
    val denominator1 = (Value(10) ^ Value(5)) * (M.expression ^ Value(0.333))

    val numerator2 = Ta.expression
    val denominator2 = T.expression

    (numerator1 / denominator1) * (numerator2 / denominator2)
  }

  override val reference: Option[String] = VolumetricEvaporation.reference

  override def identifier: UUID = VolumetricEvaporation.identifier

  override def unit: String = s"($cubic_meter_per_second)"
}

object ReleaseRateOfLiquid {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.1: Områden med explosiv gasatmosfär")
}

class ReleaseRateOfLiquid(cd: Symbol, s: Symbol, deltaP: Symbol) extends Formula with Symbols with Units {

  private val W = Symbol(Expression.Zero, "W")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${W.sign} = ${cd.sign}\\ ${s.sign}\\ \\sqrt{2\\ \\rho\\ ${deltaP.sign}}\\ $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    cd.expression * s.expression * Sqrt(Value(2) * rho.expression * deltaP.expression)
  }

  override val reference: Option[String] = ReleaseRateOfLiquid.reference

  override def identifier: UUID = ReleaseRateOfLiquid.identifier

  override def unit: String = s"($kg_per_second)"
}

object CriticalGasPressure {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.2: Områden med explosiv gasatmosfär")
}

class CriticalGasPressure(pa: Symbol, g: Symbol) extends Formula with Symbols with Units {

  private val pc = Symbol(Expression.Zero, "p_C")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${pc.sign} = ${pa.sign}\\ \\left(\\dfrac{ ${g.sign} + 1 }{2}\\right)^{ \\dfrac{ ${g.sign} }{ ${g.sign} - 1 }}\\ $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    pa.expression * (((g.expression + Value(1)) / Value(2)) ^ (g.expression / (g.expression - Value(1))))
  }

  override val reference: Option[String] = CriticalGasPressure.reference

  override def identifier: UUID = CriticalGasPressure.identifier

  override def unit: String = s"($pascal)"
}

object NonLimitedGasRate {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.3: Områden med explosiv gasatmosfär")
}

class NonLimitedGasRate(cd: Symbol, s: Symbol, M: Symbol, R: Symbol, T: Symbol, Z: Symbol, gma: Symbol, pa: Symbol, p: Symbol) extends Formula with Symbols with Units {
  private val Wg = Symbol(Expression.Zero, "W_g")

  private val texFactor1 = s"""${cd.sign} ${s.sign} ${p.sign}"""

  private val texFactor3 = s"""\\left( { \\dfrac{${pa.sign}}{${p.sign}} } \\right) ^ { \\dfrac{1}{${gma.sign}} }"""

  private val texInnerFactor1 = s"""\\dfrac{${M.sign}}{${Z.sign}${R.sign}${T.sign}}"""

  private val texInnerFactor2 = s"""\\dfrac{2 ${gamma.sign}}{${gamma.sign} - 1}"""

  private val texInnerFactor3 = s"""\\left[1 - \\left( { \\dfrac{${pa.sign}}{${p.sign}} } \\right) ^ { \\dfrac{${gamma.sign} - 1}{${gamma.sign}} } \\right]"""

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${Wg.sign} = $texFactor1 \\sqrt{ $texInnerFactor1 $texInnerFactor2 $texInnerFactor3 } $texFactor3\\ $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    val factor1 = cd.expression * s.expression * p.expression
    val factor3 = (pa.expression / p.expression) ^ (Value(1d) / gma.expression)

    val innerFactor1 = M.expression / (Z.expression * R.expression * T.expression)
    val innerFactor2 = gma.expression / (gma.expression - Value(1))
    val innerFactor3 = Value(1d) - ((pa.expression / p.expression) ^ ((gma.expression - Value(1d)) / gma.expression))

    factor1 * Sqrt(innerFactor1 * innerFactor2 *! innerFactor3) * factor3
  }

  override val reference: Option[String] = NonLimitedGasRate.reference

  override def identifier: UUID = NonLimitedGasRate.identifier

  override def unit: String = s"($kg_per_second)"
}

object ReleaseCharacter {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = None
}

class ReleaseCharacter(k: Symbol, lfl: Symbol, qg: Symbol) extends Formula with Symbols with Units {
  override val reference: Option[String] = ReleaseCharacter.reference

  val result = Symbol(Expression.Zero, "Q_{gk}")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${result.sign} = \\dfrac{${qg.sign}}{${k.sign}\\ ${lfl.sign}} $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    val numerator = qg.expression
    val denominator = k.expression * lfl.expression

    numerator / denominator
  }

  override def identifier: UUID = ReleaseCharacter.identifier

  override def unit: String = s"($cubic_meter_per_second)"
}

object ReleaseCharacter2 {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = None // TODO: Fix. I'm sure there is a reference here somewhere
}

class ReleaseCharacter2(wg: Symbol, rhoG: Symbol, k: Symbol, lfl: Symbol) extends Formula with Symbols with Units {
  val result = Symbol(Expression.Zero, "Q_{rc}")

  override val reference: Option[String] = ReleaseCharacter2.reference

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${result.sign} = \\dfrac{${wg.sign}}{${rhoG.sign}\\ ${k.sign}\\ ${lfl.sign} } $unit"""

  override def getExpression: Expression = wg.expression / (rhoG.expression * k.expression * lfl.expression)

  override def identifier: UUID = ReleaseCharacter2.identifier

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  override def unit: String = s"($cubic_meter_per_second)"
}

object VolumetricGasFlow {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.5: Områden med explosiv gasatmosfär")
}

class VolumetricGasFlow(wg: Symbol, rhoG: Symbol) extends Formula with Symbols with Units {
  private val qg = Symbol(Expression.Zero, "Q_g")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${qg.sign} = \\dfrac{${wg.sign}}{${rhoG.sign}} $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = wg.expression / rhoG.expression

  override val reference: Option[String] = VolumetricGasFlow.reference

  override def identifier: UUID = VolumetricGasFlow.identifier

  override def unit: String = s"($cubic_meter_per_second)"
}

object MolarMassToRho {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("Områden med explosiv gasatmosfär")
}

class MolarMassToRho(M: Symbol, pa: Symbol, R: Symbol, Ta: Symbol) extends Formula with Symbols with Units {
  private val rhoG = Symbol(Expression.Zero, """\rho_g""")

  override val reference: Option[String] = MolarMassToRho.reference

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${rhoG.sign} = \\dfrac{${pa.sign}${M.sign}}{${R.sign}${Ta.sign}} $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    (pa.expression * M.expression) / (R.expression * Ta.expression)
  }

  override def identifier: UUID = MolarMassToRho.identifier

  override def unit: String = s"($kg_per_cubic_meter)"
}

object LimitedGasRate {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.4: Områden med explosiv gasatmosfär")
}

class LimitedGasRate(cd: Symbol, s: Symbol, M: Symbol, R: Symbol, T: Symbol, Z: Symbol, gma: Symbol, p: Symbol) extends Formula with Symbols with Units {
  private val Wg = Symbol(Expression.Zero, "W_g")

  private val texFactor1 = s"""${cd.sign} ${s.sign} ${p.sign}"""

  private val texInnerFactor1 = s"""${gma.sign} \\dfrac{${M.sign}}{${Z.sign}${R.sign}${T.sign}}"""

  private val texInnerFactor2 = s"""\\left( { \\dfrac{2}{${gma.sign} + 1} } \\right) ^ { \\dfrac{${gamma.sign} + 1}{${gamma.sign} - 1} }"""

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${Wg.sign} = $texFactor1 \\sqrt{ $texInnerFactor1 $texInnerFactor2 }\\ $unit"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    val factor1 = cd.expression * s.expression * p.expression

    val innerFactor1 = gma.expression * (M.expression / (Z.expression * R.expression * T.expression))
    val innerFactor2 = (Value(2) / (gma.expression + Value(1))) ^ ((gma.expression + Value(1)) / (gma.expression - Value(1)))

    factor1 * Sqrt(innerFactor1 * innerFactor2)
  }

  override val reference: Option[String] = LimitedGasRate.reference

  override def identifier: UUID = LimitedGasRate.identifier

  override def unit: String = s"($kg_per_second)"
}

object PlainFormula {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = None
}

class PlainFormula(expression: Expression) extends Formula {
  override val reference: Option[String] = PlainFormula.reference

  override def texifyFormula(includeUnit: Boolean = false): String = ""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  override def getExpression: Expression = expression

  override def identifier: UUID = PlainFormula.identifier

  override def unit: String = ""
}

object BackgroundConcentrationFormulaV1 {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("C.1: Områden med explosiv gasatmosfär")
}

class BackgroundConcentrationFormulaV1(f: Symbol, Qg: Symbol, Q1: Symbol) extends Formula with Units {
  override val reference: Option[String] = BackgroundConcentrationFormulaV1.reference

  private val Xb = Symbol(Expression.Zero, "X_b")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${Xb.sign} = \\dfrac{${f.sign}${Qg.sign}}{${Qg.sign} + ${Q1.sign}} $unit"""

  override def getExpression: Expression = (f.expression * Qg.expression) / (Qg.expression + Q1.expression)

  override def identifier: UUID = BackgroundConcentrationFormulaV2.identifier

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  override def unit: String = s"($vol_vol)"
}

object BackgroundConcentrationFormulaV2 {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("C.1: Områden med explosiv gasatmosfär")
}

class BackgroundConcentrationFormulaV2(f: Symbol, Qg: Symbol, Q2: Symbol) extends Formula with Units {
  override val reference: Option[String] = BackgroundConcentrationFormulaV2.reference

  private val Xb = Symbol(Expression.Zero, "X_b")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${Xb.sign} = \\dfrac{${f.sign}${Qg.sign}}{${Q2.sign}} $unit"""

  override def getExpression: Expression = (f.expression * Qg.expression) / Q2.expression

  override def identifier: UUID = BackgroundConcentrationFormulaV2.identifier

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  override def unit: String = s"($vol_vol)"
}

object VentilationVelocityFormula {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("Table E.1, row 15: Områden med explosiv gasatmosfär")
}

class VentilationVelocityFormula(airFlow: Symbol, roomHeightSymbol: Symbol, roomLengthSymbol: Symbol) extends Formula with Units {
  override val reference: Option[String] = VentilationVelocityFormula.reference

  private val uw = Symbol(Expression.Zero, "u_w")

  override def texifyFormula(includeUnit: Boolean = false): String = s"""${uw.sign} = \\dfrac{${airFlow.sign}}{${roomHeightSymbol.sign} \\times ${roomLengthSymbol.sign}} $unit"""

  override def getExpression: Expression = airFlow.expression / (roomHeightSymbol.expression * roomLengthSymbol.expression)

  override def identifier: UUID = VentilationVelocityFormula.identifier

  override def texify(): Latex = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  override def unit: String = s"($meters_per_second)"
}
