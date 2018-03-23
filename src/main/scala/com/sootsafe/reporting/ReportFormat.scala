package com.sootsafe.reporting

trait ReportFormat {
  def write(num: BigDecimal): String
}

object DefaultReportFormat extends ReportFormat {

  private val decimalPlaces = 2

  def write(num: BigDecimal): String = {
    num.setScale(decimalPlaces, BigDecimal.RoundingMode.HALF_UP).toDouble.toString
  }
}