package com.sootsafe.reporting.figures

import java.util.UUID

import com.sootsafe.reporting.Fixture.Latex
import com.sootsafe.reporting.tex.headers.Standard

object FigureC1 extends Figure {
  override def texify(): Latex =
    s"""      \\pgfplotsset{width=15cm,compat=1.9}
       |
       |      \\begin{figure}[!hbt]
       |            \\caption{Chart for assessing the degree of dilution}
       |            \\label{fig:$identifier}
       |            \\begin{tikzpicture}
       |                  \\begin{loglogaxis}[
       |                        xmin=0.001, ymin=0.001,
       |                        xmax=100, ymax=10,
       |                        ymajorgrids=true,
       |                        xmajorgrids=true,
       |                        every axis plot/.append style={ultra thick},
       |                        xlabel={Release characteristic $$\\dfrac{W_g}{\\rho_g \\times k \\times LFL} (m^3/s)$$},
       |                        ylabel={Ventilation velocity $$u_w (m/s)$$},
       |                        ]
       |                        \\addplot[
       |                              domain=0.001:1,
       |                              samples=100,
       |                              color=blue
       |                        ]
       |                        {x * 13.5};
       |                        \\addplot[
       |                              domain=0.02:100,
       |                              samples=100,
       |                              color=red
       |                        ]
       |                        {x * 0.045};
       |                  \\end{loglogaxis}
       |
       |                  \\node [draw,fill=white] at (rel axis cs: 0.7,1.5) {\\shortstack[l]{
       |                        Dilution\\\\high
       |                  }};
       |                  \\node [draw,fill=white] at (rel axis cs: 1.1,1.25) {\\shortstack[l]{
       |                        Dilution\\\\medium
       |                  }};
       |                  \\node [draw,fill=white] at (rel axis cs: 1.5,1.0) {\\shortstack[l]{
       |                        Dilution\\\\low
       |                  }};
       |
       |            \\end{tikzpicture}
       |      \\end{figure}
    """.stripMargin

  override def texHeaders(): Seq[Latex] = Standard.PgfPlots.headers

  override val identifier: UUID = UUID.randomUUID()
}
