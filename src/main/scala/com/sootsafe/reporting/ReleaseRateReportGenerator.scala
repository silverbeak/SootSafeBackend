package com.sootsafe.reporting

import com.sootsafe.arithmetic.PlainFormula
import com.sootsafe.reporting.Fixture.Latex

object ReleaseRateReportGenerator {

  val pageBreak = """\pagebreak"""

  //  def generateCalculationChapter(calculationChapter: CalculationChapter)(implicit format: ReportFormat): Latex = {
  //    calculationChapter.calculationSectionList.map(generateCalculationSection).mkString
  //  }

  def generateCalculationSection(calculationSection: CalculationSection)(implicit format: ReportFormat): Latex = {
    val calcs = calculationSection.formulaSection.map(generateCalculationPart)
    s"""
       |${calculationSection.description.map(_.description).getOrElse("")}
       |${calcs.mkString("\n")}
       """.stripMargin
  }

  private def generateCalculationPart(fSection: FormulaSection)(implicit format: ReportFormat): Latex = {
    s"""
       |%
       |\\subsection{${fSection.description.map(_.description).getOrElse("")}}
       |${fSection.formulaContainer.map(c => s"""(Formula \\ref{${c.formula.identifier}})""").getOrElse("")}
       |\\hfill \\break
       |${
      fSection.formulaContainer.map(c =>
        s"""
           |\\par
           |\\begin{flushleft}
           |$$ ${c.formula.texify()} = ${format.write(c.formula.calculate())} $$
           |\\end{flushleft}
           |""".stripMargin).getOrElse("")
    }
            |${fSection.decision.map(_.decision).getOrElse("")}
            |%
      """.stripMargin
  }

  def generateLatex(formulaList: Seq[FormulaSection])(implicit format: ReportFormat): Latex = {
    val body = formulaList
      .filter(entry => !entry.formulaContainer.exists(c => c.formula.isInstanceOf[PlainFormula]))
      .map(generateCalculationPart)

    val uniqueFormulas = formulaList
      .filter(entry => !entry.formulaContainer.exists(c => c.formula.isInstanceOf[PlainFormula]))
      .flatMap(f => f.formulaContainer.map(_.formula.identifier -> f))
      .toMap

    val formulaSection = uniqueFormulas.map {
      case (_, entry) if entry.formulaContainer.isDefined =>
        s"""
           |%
           |\\paragraph{}
           |\\begin{equation} \\label{${entry.formulaContainer.get.formula.identifier}}
           |${entry.formulaContainer.get.formula.texifyFormula()}
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
