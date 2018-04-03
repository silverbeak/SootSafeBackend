package com.sootsafe.reporting

import java.math.MathContext

trait ReportFormat {
  def write(num: BigDecimal): String
  def write(num: Option[Double]): String
}

object DefaultReportFormat extends ReportFormat {

  private val decimalPlaces = 2
  private val significantFigures = 4

  def write(num: BigDecimal): String = {
//    num.setScale(decimalPlaces, BigDecimal.RoundingMode.HALF_UP).toDouble.toString
    num.round(new MathContext(significantFigures)).doubleValue().toString
  }

  override def write(num: Option[Double]): String = if (num.isEmpty) "" else write(num.get)
}