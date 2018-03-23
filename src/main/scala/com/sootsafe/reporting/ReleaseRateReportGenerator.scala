package com.sootsafe.reporting

import com.sootsafe.arithmetic.PlainFormula
import com.sootsafe.reporting.Fixture.Latex
import com.sootsafe.engine.zone.ReleaseRateCalculator.FormulaContainer

object ReleaseRateReportGenerator {

  val pageBreak = """\pagebreak"""

//  def generateCalculationChapter(calculationChapter: CalculationChapter)(implicit format: ReportFormat): Latex = {
//    calculationChapter.calculationSectionList.map(generateCalculationSection).mkString
//  }

  def generateCalculationSection(calculationSection: CalculationSection)(implicit format: ReportFormat): Latex = {
    s"""
       |${calculationSection.description.map(_.description)}
       |${calculationSection.formulaSection.map(fs => fs.formulaContainer.map(generateCalculationPart))}
       """.stripMargin
  }

  private def generateCalculationPart(formulaContainer: FormulaContainer)(implicit format: ReportFormat): Latex = {
    s"""
       |%
       |\\subsection{${formulaContainer.description.getOrElse("")}}
       |(Formula \\ref{${formulaContainer.formula.identifier}})
       |\\hfill \\break
       |\\par
       |\\begin{flushleft}
       |$$ ${formulaContainer.formula.texify()} = ${format.write(formulaContainer.formula.calculate())} $$
       |\\end{flushleft}
       |${formulaContainer.decision.getOrElse("")}
       |%
      """.stripMargin
  }

  def generateLatex(formulaList: Seq[FormulaContainer])(implicit format: ReportFormat): Latex = {
    val body = formulaList
      .filter(entry => !entry.formula.isInstanceOf[PlainFormula])
      .map(generateCalculationPart)

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
