package com.sootsafe.reporting

import com.sootsafe.arithmetic.PlainFormula
import com.sootsafe.reporting.Fixture.Latex

object ReleaseRateReportGenerator {

  val pageBreak = """\pagebreak"""

  //  def generateCalculationChapter(calculationChapter: CalculationChapter)(implicit format: ReportFormat): Latex = {
  //    calculationChapter.calculationSectionList.map(generateCalculationSection).mkString
  //  }

  private def generateCalculationSection(calculationSection: CalculationSection)(implicit format: ReportFormat): Latex = {
    val calcs = calculationSection.formulaSection.map(generateCalculationPart)
    s"""
       |\\subsection{${calculationSection.description.map(_.description).getOrElse("")}}
       |${calcs.mkString}
       """.stripMargin
  }

  //  ${TableC1.texify()}
  //  Table \\ref{table:${TableC1.identifier}} on page \\pageref{table:${TableC1.identifier}} refers to the...

  private def generateCalculationPart(fSection: FormulaSection)(implicit format: ReportFormat): Latex = {
    s"""
       |%
       |\\subsubsection{${fSection.description.map(_.description).getOrElse("")}}
       |${fSection.formulaContainer.filterNot(_.formula.isInstanceOf[PlainFormula]).map(c => s"""(Formula \\ref{${c.formula.identifier}})""").getOrElse("")}
       |${
      fSection.formulaContainer.filterNot(_.formula.isInstanceOf[PlainFormula]).map(c =>
        s"""
           |\\par
           |\\begin{flushleft}
           |$$ ${c.formula.texify()} = ${format.write(c.formula.calculate())}  ${c.formula.unit} $$
           |\\end{flushleft}
           |""".stripMargin).getOrElse("")
    }
       |${fSection.decision.map(_.decision).getOrElse("")}
       |%
      """.stripMargin
  }

  private def generateFormulaSection(section: Seq[FormulaSection]): Iterable[Latex] = {
    val uniqueFormulas = section
      .filter(entry => !entry.formulaContainer.exists(c => c.formula.isInstanceOf[PlainFormula]))
      .flatMap(f => f.formulaContainer.map(_.formula.identifier -> f))
      .toMap

    uniqueFormulas.map {
      case (_, entry) if entry.formulaContainer.isDefined =>
        s"""
           |\\paragraph{}
           |\\begin{equation} \\label{${entry.formulaContainer.get.formula.identifier}}
           |${entry.formulaContainer.get.formula.texifyFormula()}
           |\\end{equation}
           |\\newline
       """.stripMargin
    }
  }

  private def generateTableSection(section: Seq[FormulaSection]): Iterable[Latex] = {
    val uniqueTables = section
      .flatMap(formula => formula.tableReferences.map(table => table.identifier -> table))
      .toMap

    uniqueTables.map {
      case (_, entry) => entry.texify()
    }
  }

  private def generateFigureSection(section: Seq[FormulaSection]): Iterable[Latex] = {
    val uniqueTables = section
      .flatMap(formula => formula.figureReferences.map(table => table.identifier -> table))
      .toMap

    uniqueTables.map {
      case (_, entry) => entry.texify()
    }
  }

  def generateLatex(calculationSection: CalculationSection)(implicit format: ReportFormat): Latex = {
    val body = generateCalculationSection(calculationSection)
    val formulas = generateFormulaSection(calculationSection.formulaSection)
    val tables = generateTableSection(calculationSection.formulaSection)
    documentBuilder(Seq(body), formulas, tables)
  }

  def generateLatex(calculationSections: Seq[CalculationSection])(implicit format: ReportFormat): Latex = {
    val body = calculationSections.map(generateCalculationSection)
    val formulas = calculationSections.flatMap(calculationSection => generateFormulaSection(calculationSection.formulaSection))
    val tables = calculationSections.flatMap(calculationSection => generateTableSection(calculationSection.formulaSection))
    val figures = calculationSections.flatMap(calculationSection => generateFigureSection(calculationSection.formulaSection))
    documentBuilder(body, formulas, tables, figures)
  }

  private def documentBuilder(body: Iterable[Latex],
                              formulaSection: Iterable[Latex],
                              tableSection: Iterable[Latex] = Nil,
                              diagramSection: Iterable[Latex] = Nil): Latex = {

    val tableLatex = if (tableSection.nonEmpty) {
      pageBreak +
        "\n\\section{Tables}\n" +
        tableSection.mkString
    } else {
      ""
    }

    val diagramLatex = if (diagramSection.nonEmpty) {
      pageBreak +
        "\n\\section{Diagrams}\n" +
        diagramSection.mkString
    } else {
      ""
    }

    Fixture.head(Some("Release rate report"), Some("Jane Doe")) +
      "\n\\section{Calculations}\n" +
      body.mkString +
      pageBreak +
      "\n\\section{Formulas}\n" +
      formulaSection.mkString +
      tableLatex +
      diagramLatex +
      Fixture.end
  }

}
