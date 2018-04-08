package com.sootsafe.reporting

import java.io.File
import java.util.UUID

import scala.util.{Failure, Success, Try}

object LatexCompiler {

  def latexToPdf(filename: String, outputDirectory: String): Try[String] = {
    import sys.process._

    val identifier = UUID.randomUUID()
    val workingDirectory = s"$outputDirectory${File.separator}$identifier"
    val output = s"$workingDirectory/work"
    createWorkingDirectory(s"$workingDirectory/")
    Try(Seq("latexmk", "-synctex=1", "-interaction=nonstopmode", "-file-line-error", "-d", "-ps", "-xelatex", filename, s"-jobname=$output", "-pdflatex=pdflatex").!!.trim) match {
      case Success(_) => Success(output + ".pdf")
      case Failure(e) => Failure(new Exception(s"Could not compile TEX file.", e))
    }
  }

  private def createWorkingDirectory(dirname: String): Try[Boolean] = {
    val file = new File(dirname)
    (file.exists(), file.isDirectory) match {
      case (false, _) => Success(file.mkdir())
      case (true, false) => Failure(new Exception(s"A file with the name $dirname already exists where the working directory should be"))
      case (true, true) => Failure(new Exception(s"A directory with the name $dirname already exists"))
    }
  }

}
