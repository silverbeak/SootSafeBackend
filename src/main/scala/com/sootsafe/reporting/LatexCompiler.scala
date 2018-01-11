package com.sootsafe.reporting

import java.io.File
import java.util.UUID

import scala.util.{Failure, Success, Try}

object LatexCompiler {

  def latexToPdf(filename: String, outputDirectory: String): Try[String] = {
    import sys.process._

    val identifier = UUID.randomUUID()
    val output = s"$outputDirectory${File.separator}$identifier"
    Try(Seq("latexmk", "-synctex=1", "-interaction=nonstopmode", "-file-line-error", "-d", "-ps", "-xelatex", filename, s"-jobname=$output", "-pdflatex=pdflatex").!!.trim) match {
      case Success(_) => Success(output)
      case Failure(e) => Failure(new Exception(s"Could not compile TEX file.", e))
    }
  }

}
