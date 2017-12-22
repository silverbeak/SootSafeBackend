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

class Vaporisation(uw: Symbol, ap: Symbol, pv: Symbol, M: Symbol, R: Symbol, T: Symbol) extends Formula with Symbols with Units {

  private val we = Symbol(Expression.Zero, "W_e")

  override def texifyFormula(): String = s"""${we.sign} = \\dfrac{6,55\\ ${uw.sign}^{0,78}\\ ${ap.sign}\\ ${pv.sign}\\ ${M.sign}^{0,667}}{${R.sign}\\ ${T.sign}} ($kg_per_second)"""

  override def texify(): String = getExpression.texify()

  override def calculate(): Double = getExpression.calculate()

  private def getExpression: Expression = {
    val numerator = Value(6.55) * (uw.expression ^ Value(0.78)) * ap.expression * pv.expression * (M.expression ^ Value(0.667))
    val denominator = R.expression * T.expression
    numerator / denominator
  }
}