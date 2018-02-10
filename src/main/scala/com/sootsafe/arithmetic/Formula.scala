package com.sootsafe.arithmetic

import java.util.UUID

import com.sootsafe.model.NodeModule

trait FormulaParameter

trait Formula extends Expression {

  val reference: Option[String]

  def texifyFormula(): String

  def getExpression: Expression

  def identifier: UUID
}

object DynamicPressure {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = None
}

class DynamicPressure(nodeModule: NodeModule) extends Formula with Symbols {

  private val capacity = Value(nodeModule.ssInfo.capacity.get / 1000)

  private val diameter = Value(nodeModule.ssInfo.dimension.diameter.get / 1000)

  override def texifyFormula(): String ="""\zeta = \dfrac{ \pi^{2}d^{4} \Delta p_j } { 8 \rho q^{2} }"""

  override def texify(): String = getExpression.texify()

  def getExpression: Expression = {
    val numerator = Value(8) * rho.expression * (capacity ^ Value(2))
    val denominator = (pi.expression ^ Value(2)) * (diameter ^ Value(4))
    numerator / denominator
  }

  override def calculate(): Double = getExpression.calculate()

  override val reference: Option[String] = DynamicPressure.reference

  override def identifier: UUID = DynamicPressure.identifier
}

object Evaporation {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.6: Områden med explosiv gasatmosfär")
}

class Evaporation(uw: Symbol, ap: Symbol, pv: Symbol, M: Symbol, R: Symbol, T: Symbol) extends Formula with Symbols with Units {

  private val we = Symbol(Expression.Zero, "W_e")

  override def texifyFormula(): String = s"""${we.sign} = \\dfrac{6,55\\ ${uw.sign}^{0,78}\\ ${ap.sign}\\ ${pv.sign}\\ ${M.sign}^{0,667}}{${R.sign}\\ ${T.sign}}\\ ($kg_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    val numerator = Value(6.55) * (uw.expression ^ Value(0.78)) * ap.expression * pv.expression * (M.expression ^ Value(0.667))
    val denominator = R.expression * T.expression
    numerator / denominator
  }

  override val reference: Option[String] = Evaporation.reference

  override def identifier: UUID = Evaporation.identifier
}

object VolumetricEvaporation {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.7: Områden med explosiv gasatmosfär")
}

class VolumetricEvaporation(uw: Symbol, ap: Symbol, pv: Symbol, M: Symbol, T: Symbol, Ta: Symbol) extends Formula with Symbols with Units {

  private val Qg = Symbol(Expression.Zero, "Q_g")

  override def texifyFormula(): String = s"""${Qg.sign} \\approx \\dfrac{6,5\\ ${uw.sign}^{0,78}\\ ${ap.sign}\\ ${pv.sign}}{10^5\\ ${M.sign}^{0,333}} \\times \\dfrac {${Ta.sign}}{${T.sign}}\\ ($cubic_meter_per_second)"""

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
}

object ReleaseRateOfLiquid {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.1: Områden med explosiv gasatmosfär")
}

class ReleaseRateOfLiquid(cd: Symbol, s: Symbol, deltaP: Symbol) extends Formula with Symbols with Units {

  private val W = Symbol(Expression.Zero, "W")

  override def texifyFormula(): String = s"""${W.sign} = ${cd.sign}\\ ${s.sign}\\ \\sqrt{2\\ \\rho\\ ${deltaP.sign}}\\ ($kg_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    cd.expression * s.expression * Sqrt(Value(2) * rho.expression * deltaP.expression)
  }

  override val reference: Option[String] = ReleaseRateOfLiquid.reference

  override def identifier: UUID = ReleaseRateOfLiquid.identifier
}

object CriticalGasPressure {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.2: Områden med explosiv gasatmosfär")
}

class CriticalGasPressure(pa: Symbol, g: Symbol) extends Formula with Symbols with Units {

  private val pc = Symbol(Expression.Zero, "p_C")

  override def texifyFormula(): String = s"""${pc.sign} = ${pa.sign}\\ \\left(\\dfrac{ ${g.sign} + 1 }{2}\\right)^{ \\dfrac{ ${g.sign} }{ ${g.sign} - 1 }}\\ ($pascal)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    pa.expression * (((g.expression + Value(1)) / Value(2)) ^ (g.expression / (g.expression - Value(1))))
  }

  override val reference: Option[String] = CriticalGasPressure.reference

  override def identifier: UUID = CriticalGasPressure.identifier
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

  override def texifyFormula(): String = s"""${Wg.sign} = $texFactor1 \\sqrt{ $texInnerFactor1 $texInnerFactor2 $texInnerFactor3 } $texFactor3\\ ($kg_per_second)"""

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
}

object ReleaseCharacter {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = None
}

class ReleaseCharacter(k: Symbol, lfl: Symbol, qg: Symbol) extends Formula with Symbols with Units {
  override val reference: Option[String] = ReleaseCharacter.reference

  val result = Symbol(Expression.Zero, "Q_{gk}")

  override def texifyFormula(): String = s"""${result.sign} = \\dfrac{${qg.sign}}{${k.sign}\\ ${lfl.sign}} ($cubic_meter_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    val numerator = qg.expression
    val denominator = k.expression * lfl.expression

    numerator / denominator
  }

  override def identifier: UUID = ReleaseCharacter.identifier
}

object VolumetricGasFlow {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("B.5: Områden med explosiv gasatmosfär")
}

class VolumetricGasFlow(wg: Symbol, rhoG: Symbol) extends Formula with Symbols with Units {
  private val qg = Symbol(Expression.Zero, "Q_g")

  override def texifyFormula(): String = s"""${qg.sign} = \\dfrac{${wg.sign}}{${rhoG.sign}} ($cubic_meter_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = wg.expression / rhoG.expression

  override val reference: Option[String] = VolumetricGasFlow.reference

  override def identifier: UUID = VolumetricGasFlow.identifier
}

object MolarMassToRho {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("Områden med explosiv gasatmosfär")
}

class MolarMassToRho(M: Symbol, pa: Symbol, R: Symbol, Ta: Symbol) extends Formula with Symbols with Units {
  private val rhoG = Symbol(Expression.Zero, """\rho_g""")

  override val reference: Option[String] = MolarMassToRho.reference

  override def texifyFormula(): String = s"""${rhoG.sign} = \\dfrac{${pa.sign}${M.sign}}{${R.sign}${Ta.sign}} ($kg_per_cubic_meter)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  def getExpression: Expression = {
    (pa.expression * M.expression) / (R.expression * Ta.expression)
  }

  override def identifier: UUID = MolarMassToRho.identifier
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

  override def texifyFormula(): String = s"""${Wg.sign} = $texFactor1 \\sqrt{ $texInnerFactor1 $texInnerFactor2 }\\ ($kg_per_second)"""

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

}

object PlainFormula {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = None
}

class PlainFormula(expression: Expression) extends Formula {
  override val reference: Option[String] = PlainFormula.reference

  override def texifyFormula(): String = ""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  override def getExpression: Expression = expression

  override def identifier: UUID = PlainFormula.identifier
}

object BackgroundConcentrationFormula {
  val identifier: UUID = UUID.randomUUID()

  val reference: Option[String] = Some("C.1: Områden med explosiv gasatmosfär")
}

class BackgroundConcentrationFormula(f: Symbol, Qg: Symbol, Q2: Symbol) extends Formula with Units {
  override val reference: Option[String] = BackgroundConcentrationFormula.reference

  private val Xb = Symbol(Expression.Zero, "X_b")

  override def texifyFormula(): String = s"""${Xb.sign} = \\dfrac{${f.sign}${Qg.sign}}{${Q2.sign}} ($vol_vol)"""

  override def getExpression: Expression = (f.expression * Qg.expression) / Q2.expression

  override def identifier: UUID = BackgroundConcentrationFormula.identifier

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()
}
