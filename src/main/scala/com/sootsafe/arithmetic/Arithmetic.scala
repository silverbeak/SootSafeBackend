package com.sootsafe.arithmetic

object Arithmetic {

  def texify(expression: Expression): String =
    s"""
       |\\documentclass[12pt]{article}
       |    \\usepackage{amsmath}
       |    \\usepackage{graphicx}
       |    \\usepackage{hyperref}
       |    \\usepackage[latin1]{inputenc}
       |    \\usepackage{mathtools}
       |
       |    \\DeclarePairedDelimiter\\abs{\\lvert}{\\rvert}%
       |
       |    \\makeatletter
       |    \\let\\oldabs\\abs
       |    \\def\\abs{\\@ifstar{\\oldabs}{\\oldabs*}}
       |
       |    \\title{Getting started}
       |    \\author{Veloci Raptor}
       |    \\date{\\today}
       |
       |    \\begin{document}
       |    \\maketitle
       |
       |    \\ ${expression.texify()}
       |
       |\\end{document}
       |
     """.stripMargin

}

trait Expression {
  def texify(): String

  def calculate(): Double

  def toValue: Value = Value(this.calculate())

  def +(that: Expression): Expression = Addition(this, that)

  def -(that: Expression): Expression = Subtraction(this, that)

  def *(that: Expression): Expression = Multiplication(this, that)

  def *!(that: Expression): Expression = Multiplication(this, that, (false, true))

  def !*!(that: Expression): Expression = Multiplication(this, that, (true, true))

  def !*(that: Expression): Expression = Multiplication(this, that, (true, false))

  def /(that: Expression): Expression = Division(this, that)

  def ^(that: Expression): Expression = Power(this, that)

  def >(that: Expression): Boolean = this.calculate() > that.calculate()

  def <(that: Expression): Boolean = this.calculate() < that.calculate()
}

object Expression {
  val Zero: Expression = Value(0)
}

case class Empty() extends Expression {
  override def texify(): String = ""

  override def calculate(): Double = 0
}

case class Value(value: Double) extends Expression {
  override def texify(): String = s"$value"

  override def calculate(): Double = value
}

case class Multiplication(factor1: Expression, factor2: Expression, forceParenthesis: (Boolean, Boolean) = (false, false)) extends Expression {
  override def texify(): String = forceParenthesis match {
    case (true, true) => s"\\left(${factor1.texify()}\\right) \\times \\left(${factor2.texify()}\\right)"
    case (true, false) => s"\\left(${factor1.texify()}\\right) \\times ${factor2.texify()}"
    case (false, true) => s"${factor1.texify()} \\times \\left(${factor2.texify()}\\right)"
    case _ => s"${factor1.texify()} \\times ${factor2.texify()}"
  }

  override def calculate(): Double = factor1.calculate() * factor2.calculate()
}

case class Subtraction(term1: Expression, term2: Expression) extends Expression {
  override def texify(): String = s"${term1.texify()} - ${term2.texify()}"

  override def calculate(): Double = term1.calculate() - term2.calculate()
}

case class Addition(term1: Expression, term2: Expression) extends Expression {
  override def texify(): String = s"${term1.texify()} + ${term2.texify()}"

  override def calculate(): Double = term1.calculate() + term2.calculate()
}

case class Sqrt(root: Expression) extends Expression {
  override def texify(): String = s"\\sqrt{${root.texify()}}"

  override def calculate(): Double = Math.sqrt(root.calculate())
}

case class Division(dividend: Expression, divisor: Expression) extends Expression {
  override def texify(): String = s"\\dfrac{${dividend.texify()}}{${divisor.texify()}}"

  override def calculate(): Double = dividend.calculate() / divisor.calculate()
}

case class Power(term: Expression, power: Expression) extends Expression {
  override def texify(): String = term match {
    case _: Value => s"{${term.texify()}}^{${power.texify()}}"
    case _ => s"{\\left({${term.texify()}}\\right)}^{${power.texify()}}"
  }

  override def calculate(): Double = Math.pow(term.calculate(), power.calculate())
}

case class Absolute(value: Expression) extends Expression {
  override def texify(): String = s"\\abs{${value.texify()}}"

  override def calculate(): Double = Math.abs(value.calculate())
}
