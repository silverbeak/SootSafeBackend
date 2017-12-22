package com.sootsafe.arithmetic

import com.sootsafe.model.NodeModule

trait FormulaParameter

trait Formula extends Expression {

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

    (numerator1/denominator1) * (numerator2/denominator2)
  }
}

class ReleaseRateOfLiquid(cd: Symbol, s: Symbol, deltaP: Symbol) extends Formula with Symbols with Units {

  private val W = Symbol(Expression.Zero, "W")

  override def texifyFormula(): String = s"""${W.sign} = ${cd.sign}\\ ${s.sign}\\ \\sqrt{2\\ \\rho\\ ${deltaP.sign}}\\ ($kg_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  private def getExpression: Expression = {
    cd.expression * s.expression * Sqrt(Value(2) * rho.expression * deltaP.expression)
  }
}