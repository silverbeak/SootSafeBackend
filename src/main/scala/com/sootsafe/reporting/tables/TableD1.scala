package com.sootsafe.reporting.tables

import java.util.UUID

import com.sootsafe.reporting.Fixture.Latex
import com.sootsafe.reporting.tex.headers.Standard

object TableD1 extends Table {
  override def texify(): Latex =
    s"""
       |       \\begin{table}[!hbt]
       |            \\caption{Zones for grade of release and effectiveness of ventilation}
       |            \\small
       |            \\newcolumntype{L}[1]{>{\\raggedright\\arraybackslash}p{#1}}
       |            \\newcolumntype{C}[1]{>{\\centering\\arraybackslash}p{#1}}
       |            \\begin{threeparttable}
       |                  \\begin{tabular}{ |C{2cm}|C{2cm}|C{2cm}|C{2cm}|C{2cm}|C{2cm}|C{2cm}|C{2cm}| }
       |                        \\hline
       |                        \\multirow{4}{*}{Grade of release}
       |                        &\\multicolumn{7}{|c|}{Effectiveness of ventilation}\\\\
       |                        \\cline{2-8}
       |                        &\\multicolumn{3}{|c|}{High Dilution}
       |                        &\\multicolumn{3}{|c|}{Medium Dilution}
       |                        &Low Dilution\\\\
       |                        \\cline{2-8}
       |                        &\\multicolumn{7}{|c|}{Availability of ventilation}\\\\
       |                        \\cline{2-8}
       |                        &Good
       |                        &Fair
       |                        &Poor
       |                        &Good
       |                        &Fair
       |                        &Poor
       |                        &Good, fair or poor\\\\
       |                        \\hline
       |
       |                  Continuous
       |                        &Non-hazardous (Zone 0 NE)\\tnote{a}
       |                        &Zone 2 (Zone 0 NE)\\tnote{a}
       |                        &Zone 1 (Zone 0 NE)\\tnote{a}
       |                        &Zone 0
       |                        &Zone 0 + Zone 2
       |                        &Zone 0 + Zone 1
       |                        &Zone 0\\\\
       |                        \\hline
       |
       |                  Primary
       |                        &Non-hazardous (Zone 1 NE)\\tnote{a}
       |                        &Zone 2 (Zone 1 NE)\\tnote{a}
       |                        &Zone 2 (Zone 1 NE)\\tnote{a}
       |                        &Zone 1
       |                        &Zone 1 + Zone 2
       |                        &Zone 1 + Zone 2
       |                        &Zone 1 or Zone 0\\tnote{c}\\\\
       |                        \\hline
       |
       |                  Secondary\\tnote{b}
       |                        &Non-hazardous (Zone 2 NE)\\tnote{a}
       |                        &Non-hazardous (Zone 2 NE)\\tnote{a}
       |                        &Zone 2
       |                        &Zone 2
       |                        &Zone 2
       |                        &Zone 2
       |                        &Zone 1 and even Zone 0\\tnote{c}\\\\
       |                        \\hline
       |
       |                  \\end{tabular}
       |                  \\begin{tablenotes}\\footnotesize
       |                        \\item [a]
       |                              Zone 0 NE, 1 NE or 2 NE indicates a theoretical zone which would be of negligible extent under normal contitions.
       |                        \\item [b]
       |                              The zone 2 area created by a secondary grade of release may exceed that attributable to a primary or continuous grade of release; in this case, the greater distance should be taken.
       |                        \\item [c]
       |                              Will be zone 0 if the ventilation is so weak and the release is such that in practice an explosive gas atmosphere exists virtually continuously (i.e. approaching a ’no ventilation‘ condition).
       |                        \\item [+]
       |                              Signifies 'surrounded by' \\\\
       |                              Availability of ventilation in naturally ventilated enclosed spaces shall never be considered as good.
       |                  \\end{tablenotes}
       |            \\end{threeparttable}
       |            \\label{table:$identifier}
       |      \\end{table}
    """.stripMargin

  override def texHeaders(): Seq[Latex] = Standard.ThreePartTable.headers ++ Standard.MultiRow.headers

  override val identifier: UUID = UUID.randomUUID()
}
