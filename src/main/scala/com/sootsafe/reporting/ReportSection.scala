package com.sootsafe.reporting

import com.sootsafe.engine.zone.ReleaseRateCalculator.FormulaContainer
import com.sootsafe.reporting.Fixture.Latex

trait ReportSection {
  def texify(): Latex
}

case class CalculationChapter(calculationSectionList: Seq[ReportSection]) extends ReportSection {
  private val spacer: Latex = "XXX"

  override def texify(): Latex = calculationSectionList.mkString(spacer)
}

case class CalculationSection(description: Option[Description], formulaSection: Seq[FormulaSection]) extends ReportSection {
  private val descriptionSpacer: Latex = "XXX"
  private val formulaSpacer: Latex = "XXX"

  override def texify(): Latex = {
    description.map(_.texify() + descriptionSpacer).getOrElse("") +
      formulaSection.map(_.texify() + formulaSpacer)
  }
}

case class AppendixSection(formulaList: Seq[FormulaCalculation]) extends ReportSection {
  private val spacer: Latex = "XXX"

  override def texify(): Latex = formulaList.mkString(spacer)
}

case class Description(description: String) extends ReportSection {
  override def texify(): Latex = description
}

case class Decision(decision: String) extends ReportSection {
  override def texify(): Latex = decision
}

case class FormulaCalculation(formula: FormulaContainer) extends ReportSection {
  override def texify(): Latex = formula.formula.texify()
}

case class FormulaDefinition(formula: FormulaContainer) extends ReportSection {
  override def texify(): Latex = formula.formula.texifyFormula() // TODO: Add reference/description somewhere?
}

case class FormulaSection(formulaContainer: Option[FormulaContainer], decision: Option[Decision], description: Option[Description]) extends ReportSection {
  private val descriptionSpacer: Latex = "XXX"
  private val decisionSpacer: Latex = "XXX"
  private val formulaSpacer: Latex = "XXX"

  override def texify(): Latex = {
    description.map(_.texify() + descriptionSpacer).getOrElse("") +
      decision.map(_.texify() + decisionSpacer) +
      formulaContainer.map(_.formula.texify() + formulaSpacer)
  }

  def calculate(): Option[Double] = formulaContainer.map(_.formula.calculate())
}
