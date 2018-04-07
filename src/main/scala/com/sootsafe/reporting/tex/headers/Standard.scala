package com.sootsafe.reporting.tex.headers

import com.sootsafe.reporting.Fixture.Latex

trait Headers extends Enumeration {

  case class Val(headers: Seq[Latex]) extends super.Val

}

object Standard extends Headers {
  val BoldLine = Val(Seq("""\\usepackage{boldline}"""))
  val Array = Val(Seq("""\\usepackage{array}"""))
  val MultiRow = Val(Seq("""\\usepackage{multirow}"""))
  val ThreePartTable = Val(Seq("""\\usepackage{threeparttable}"""))
  val PgfPlots = Val(Seq("""\\usepackage{pgfplots}"""))
}
