package com.sootsafe.reporting

import com.sootsafe.engine.FlowAndPressure
import com.sootsafe.reporting.Fixture.Latex

object SootSafeReportGenerator {

  def generateLatex(flowAndPressureList: Seq[FlowAndPressure]): Latex = {
    val body = flowAndPressureList.map(entry =>
      s"""
         |%
         |\\subsection{Punkt ${entry.junction.nodeModule.key}}
         |\\paragraph{Aggregated regular flow}
         |$$ ${entry.aggregatedRegularFlow.texify()} $$
         |%
         |\\paragraph{Aggregated fire flow}
         |$$ ${entry.aggregatedFireFlow.texify()} $$
         |%
         |\\paragraph{Fire pressure difference}
         |$$ ${entry.firePressureDifference.texify()} $$
         |%
      """.stripMargin)

    Fixture.head(Some("Fläkt i drift - kalkyldokument"), Some("John Doe")) + body.mkString + Fixture.end
  }

}
