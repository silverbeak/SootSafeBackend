package com.sootsafe.reporting.tables

import java.util.UUID

import com.sootsafe.reporting.Fixture.Latex
import com.sootsafe.reporting.tex.headers.Standard

object TableB2 extends Table {
  override def texify(): Latex =
    s"""
       |      \begin{table}[!hbt]
       |            \caption{Effect of hazardous zones on openings as possible sources of release}
       |            \newcolumntype{L}[1]{>{\raggedright\arraybackslash}p{#1}}
       |            \begin{tabular}{ |c|c|l|  }
       |                  \hlineB{3}
       |                  Zone upstream of opening&Opening type&Grade of release of openings considered as sources of release\\
       |                  \hlineB{3}
       |                  \multirow{4}{*}{Zone 0}
       |                  &A&Continuous\\
       |                  &B&(Continuous)/primary\\
       |                  &C&Secondary\\
       |                  &D&Secondary/no release\\
       |                  \hlineB{3}
       |                  \multirow{4}{*}{Zone 1}
       |                  &A&Primary\\
       |                  &B&(Primary)/secondary\\
       |                  &C&(Secondary)/no release\\
       |                  &D&No release\\
       |                  \hlineB{3}
       |                  \multirow{4}{*}{Zone 2}
       |                  &A&Secondary\\
       |                  &B&(Secondary)/no release\\
       |                  &C&No release\\
       |                  &D&No release\\
       |                  \hlineB{3}
       |                  \multicolumn{3}{|L{15cm}|}{For grades of release shown in brackets, the frequency of operation of the openings should be considered in the design.}\\
       |                  \hlineB{3}
       |            \end{tabular}
       |            \label{table:$identifier}
       |      \end{table}
    """.stripMargin

  override def texHeaders(): Seq[Latex] = Standard.BoldLine.headers ++ Standard.MultiRow.headers ++ Standard.Array.headers

  override val identifier: UUID = UUID.randomUUID()
}
