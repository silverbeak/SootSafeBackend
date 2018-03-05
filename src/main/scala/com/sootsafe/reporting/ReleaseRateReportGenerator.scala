package com.sootsafe.reporting

import com.sootsafe.arithmetic.PlainFormula
import com.sootsafe.reporting.Fixture.Latex
import com.sootsafe.engine.zone.ReleaseRateCalculator.FormulaContainer

object ReleaseRateReportGenerator {

  val pageBreak = """\pagebreak"""

  def generateLatex(formulaList: Seq[FormulaContainer]): Latex = {
    val body = formulaList
      .filter(entry => !entry.formula.isInstanceOf[PlainFormula])
      .map(entry =>
        s"""
           |%
           |\\subsection{${entry.description.getOrElse("")}}
           |(Formula \\ref{${entry.formula.identifier}})
           |\\hfill \\break
           |\\par
           |\\begin{flushleft}
           |$$ ${entry.formula.texify()} = ${entry.formula.calculate()} $$
           |\\end{flushleft}
           |%
      """.stripMargin)

    val uniqueFormulas = formulaList
      .filter(entry => !entry.formula.isInstanceOf[PlainFormula])
      .map(f => f.formula.identifier -> f)
      .toMap

    val formulaSection = uniqueFormulas.map {
      case (_, entry) =>
        s"""
           |%
           |\\paragraph{}
           |\\begin{equation} \\label{${entry.formula.identifier}}
           |${entry.formula.texifyFormula()}
           |\\end{equation}
           |\\newline
           |%
       """.stripMargin
    }

    Fixture.head(Some("Release rate report"), Some("Jane Doe")) +
      "\n\\section{Calculations}\n" +
      body.mkString +
      pageBreak +
      "\n\\section{Formulas used}\n" +
      formulaSection.mkString +
      Fixture.end
  }

}
