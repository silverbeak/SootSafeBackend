package com.sootsafe.arithmetic

import com.sootsafe.model.NodeModule

trait FormulaParameter

trait Formula extends Expression {

  val reference: Option[String]

  def texifyFormula(): String

}

class DynamicPressure(nodeModule: NodeModule) extends Formula with Symbols {

  private val capacity = Value(nodeModule.ssInfo.capacity.get / 1000)

  private val diameter = Value(nodeModule.ssInfo.dimension.diameter.get / 1000)

  override def texifyFormula(): String ="""\zeta = \dfrac{ \pi^{2}d^{4} \Delta p_j } { 8 \rho q^{2} }"""

  override def texify(): String = getExpression.texify()

  private def getExpression: Expression = {
    val numerator = Value(8) * rho.expression * (capacity ^ Value(2))
    val denominator = (pi.expression ^ Value(2)) * (diameter ^ Value(4))
    numerator / denominator
  }

  override def calculate(): Double = getExpression.calculate()

  override val reference: Option[String] = None
}

class Evaporation(uw: Symbol, ap: Symbol, pv: Symbol, M: Symbol, R: Symbol, T: Symbol) extends Formula with Symbols with Units {

  private val we = Symbol(Expression.Zero, "W_e")

  override def texifyFormula(): String = s"""${we.sign} = \\dfrac{6,55\\ ${uw.sign}^{0,78}\\ ${ap.sign}\\ ${pv.sign}\\ ${M.sign}^{0,667}}{${R.sign}\\ ${T.sign}}\\ ($kg_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  private def getExpression: Expression = {
    val numerator = Value(6.55) * (uw.expression ^ Value(0.78)) * ap.expression * pv.expression * (M.expression ^ Value(0.667))
    val denominator = R.expression * T.expression
    numerator / denominator
  }

  override val reference: Option[String] = Some("B.5: Områden med explosiv gasatmosfär")
}

class VolumetricEvaporation(uw: Symbol, ap: Symbol, pv: Symbol, M: Symbol, T: Symbol, Ta: Symbol) extends Formula with Symbols with Units {

  private val Qg = Symbol(Expression.Zero, "Q_g")

  override def texifyFormula(): String = s"""${Qg.sign} \\approx \\dfrac{6,5\\ ${uw.sign}^{0,78}\\ ${ap.sign}\\ ${pv.sign}}{10^5\\ ${M.sign}^{0,333}} \\times \\dfrac {${Ta.sign}}{${T.sign}}\\ ($cubic_meter_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  private def getExpression: Expression = {
    val numerator1 = Value(6.5) * (uw.expression ^ Value(0.78)) * ap.expression * pv.expression
    val denominator1 = (Value(10) ^ Value(5)) * (M.expression ^ Value(0.333))

    val numerator2 = Ta.expression
    val denominator2 = T.expression

    (numerator1 / denominator1) * (numerator2 / denominator2)
  }

  override val reference: Option[String] = Some("B.5: Områden med explosiv gasatmosfär")
}

class ReleaseRateOfLiquid(cd: Symbol, s: Symbol, deltaP: Symbol) extends Formula with Symbols with Units {

  private val W = Symbol(Expression.Zero, "W")

  override def texifyFormula(): String = s"""${W.sign} = ${cd.sign}\\ ${s.sign}\\ \\sqrt{2\\ \\rho\\ ${deltaP.sign}}\\ ($kg_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  private def getExpression: Expression = {
    cd.expression * s.expression * Sqrt(Value(2) * rho.expression * deltaP.expression)
  }

  override val reference: Option[String] = Some("B.1: Områden med explosiv gasatmosfär")
}

class CriticalGasPressure(pa: Symbol, g: Symbol) extends Formula with Symbols with Units {

  private val pc = Symbol(Expression.Zero, "p_C")

  override def texifyFormula(): String = s"""${pc.sign} = ${pa.sign}\\ \\left(\\dfrac{ ${g.sign} + 1 }{2}\\right)^{ \\dfrac{ ${g.sign} }{ ${g.sign} - 1 }}\\ ($pascal)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  private def getExpression: Expression = {
    pa.expression * (((g.expression + Value(1)) / Value(2)) ^ (g.expression / (g.expression - Value(1))))
  }

  override val reference: Option[String] = Some("B.2: Områden med explosiv gasatmosfär")
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

  private def getExpression: Expression = {
    val factor1 = cd.expression * s.expression * p.expression
    val factor3 = (pa.expression / p.expression) ^ (Value(1d) / gma.expression)

    val innerFactor1 = M.expression / (Z.expression * R.expression * T.expression)
    val innerFactor2 = gma.expression / (gma.expression - Value(1))
    val innerFactor3 = Value(1d) - ((pa.expression / p.expression) ^ ((gma.expression - Value(1d)) / gma.expression))

    factor1 * Sqrt(innerFactor1 * innerFactor2 *! innerFactor3) * factor3
  }

  override val reference: Option[String] = Some("B.3: Områden med explosiv gasatmosfär")
}
