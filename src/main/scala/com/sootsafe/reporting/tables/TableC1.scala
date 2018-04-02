package com.sootsafe.reporting.tables

import com.sootsafe.reporting.Fixture.Latex
import com.sootsafe.reporting.Texifyable
import com.sootsafe.reporting.tex.headers.Standard

object TableC1 extends Texifyable {
  override def texify(): Latex =
    """
      |\newcolumntype{?}{!{\vrule width 1pt}}
      |\begin{tabular}{ ?p{5cm}?c|p{2cm}|c?c|p{2cm}|c?  }
      |      \hlineB{3}
      |      Type of outdoor locations& \multicolumn{3}{c|}{Unobstructed areas}& \multicolumn{3}{c|}{Obstructed areas}\\
      |      \hlineB{3}
      |      Elevation from ground level& $\leq$ 2&> 2 m up to 5 m&> 5 m &$\leq$ 2 m&> 2 m up to 5 m&> 5 m\\
      |      \hline
      |      Indicative ventilation velocities for estimating the dilution of lighter than air gas/vapour releases&0,5 m/s&1 m/s&2 m/s&0,5 m/s&0,5 m/s&1 m/s\\
      |      \hline
      |      Indicative ventilation velocities for estimating the dilution of heavier than air gas/vapour releases&0,3 m/s&0,6 m/s&1 m/s&0,15 m/s&0,3 m/s&1 m/s\\
      |      \hline
      |      Indicative ventilation velocities for estimating the liquid pool evaporation rate at any elevation& \multicolumn{3}{c|}{> 0,25 m/s}& \multicolumn{3}{c|}{> 0,1 m/s}\\
      |      \hlineB{3}
      |\end{tabular}
    """.stripMargin

  override def texHeaders(): Seq[Latex] = Standard.BoldLine.headers
}
