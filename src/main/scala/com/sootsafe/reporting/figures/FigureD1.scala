package com.sootsafe.reporting.figures

import java.util.UUID

import com.sootsafe.reporting.Fixture.Latex
import com.sootsafe.reporting.tex.headers.Standard

object FigureD1 extends Figure {
  override def texify(): Latex =
    s"""
       |      \\pgfplotsset{width=15cm,compat=1.9}
       |      \\begin{figure}[!hbt]
       |            \\caption{Chart for estimating hazardous area distances}
       |            \\label{fig:$identifier}
       |            \\begin{tikzpicture}
       |                  \\begin{loglogaxis}[
       |                        xmin=0.01, ymin=1,
       |                        xmax=100, ymax=100,
       |                        ymajorgrids=true,
       |                        xmajorgrids=true,
       |                        legend pos=north west,
       |                        every axis plot/.append style={ultra thick},
       |                        xlabel={Release characteristic $$\\dfrac{W_g}{\\rho_g \\times k \\times LFL} (m^3/s)$$},
       |                        ylabel={Hazardous distance  $$(m)$$},
       |                        ]
       |                        \\addplot[
       |                              domain=0.03:40,
       |                              samples=100,
       |                              color=red
       |                        ]
       |                        {9*sqrt(x)};
       |                        \\addlegendentry{Heavy gas}
       |                        \\addplot[
       |                              domain=0.05:40,
       |                              samples=100,
       |                              color=blue
       |                        ]
       |                        {4.2*sqrt(x)};
       |                        \\addlegendentry{Diffusive}
       |                        \\addplot[
       |                              domain=0.2:40,
       |                              samples=100,
       |                              color=green
       |                        ]
       |                        {1.8*sqrt(x)};
       |                        \\addlegendentry{Jet}
       |                  \\end{loglogaxis}
       |            \\end{tikzpicture}
       |      \\end{figure}
    """.stripMargin

  override def texHeaders(): Seq[Latex] = Standard.PgfPlots.headers

  override val identifier: UUID = UUID.randomUUID()
}
