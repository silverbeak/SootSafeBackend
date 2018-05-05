package com.sootsafe.reporting

import com.sootsafe.reporting.Fixture.Latex

trait Texifyable {
  def texify(): Latex

  def texHeaders(): Seq[Latex] = Nil
}

trait Referable

object Fixture {

  type Latex = String

  def head(title: Option[String], author: Option[String]): String =
    s"""|\\documentclass[10pt, letterpaper]{article}
        |\\usepackage{amsmath}
        |\\usepackage{graphicx}
        |\\usepackage[utf8]{inputenc}
        |\\usepackage{mathtools}
        |\\usepackage{physics}
        |\\usepackage{siunitx}
        |\\usepackage[margin=0.8in]{geometry}
        |\\usepackage[printwatermark]{xwatermark}
        |\\usepackage{boldline}
        |\\usepackage{pgfplots}
        |\\usepackage{threeparttable}
        |\\usepackage{multirow}
        |% Change base font for entire document to latin modern
        |\\usepackage{lmodern}
        |\\renewcommand{\\familydefault}{\\sfdefault}   % Supprime le serif (dyslexie)
        |\\usepackage[font=sf, labelfont={sf}]{caption}
        |% End font update
        |
        |\\newwatermark*[allpages,color=red!50,angle=45,scale=3,xpos=0,ypos=0]{SOOTSAFE.COM}
        |
        |% \\DeclarePairedDelimiter\\abs{\\lvert}{\\rvert}%
        |%
        |\\sisetup{round-mode=places,round-precision=2}
        |%
        |\\makeatletter
        |% \\let\\oldabs\\abs
        |% \\def\\abs{\\@ifstar{\\oldabs}{\\oldabs*}}
        |%
        |\\title{${title.getOrElse("")}}
        |\\author{${author.getOrElse("")}}
        |\\date{\\today}
        |%
        |\\begin{document}
        |%
        |\\begin{titlepage}
        |\\maketitle
        |\\thispagestyle{empty}
        |\\end{titlepage}
        |%"""
      .stripMargin

  val end: String =
    s"""
       |%
       |\\end{document}"""
      .stripMargin
}
