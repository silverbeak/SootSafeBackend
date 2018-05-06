package com.sootsafe.reporting

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Date

import com.google.protobuf.ByteString
import com.sootsafe.latex.TexToPdfGrpc
import com.sootsafe.latex.TexToPdfService.TexMessage
import com.sootsafe.reporting.Fixture.Latex
import io.grpc.ManagedChannelBuilder

import scala.util.{Failure, Success, Try}

trait TexToPdfGenerator {
  def generate(payload: Latex): Try[Array[Byte]]
}

class PdfGeneratorServiceClient(address: String, port: Int) extends TexToPdfGenerator {

  private val channelBuilder = ManagedChannelBuilder.forAddress(address, port).usePlaintext(true)
  private val channel = channelBuilder.build()
  private val blockingStub = TexToPdfGrpc.newBlockingStub(channel)
  //  private val asyncStub = TexToPdfGrpc.newStub(channel)

  def generate(payload: Latex): Try[Array[Byte]] = {
    val request = TexMessage.newBuilder().setTex(ByteString.copyFrom(payload.getBytes)).build()
    Try(blockingStub.convert(request)).map(_.getPdf.toByteArray)
  }
}

class PdfGeneratorLocal() extends TexToPdfGenerator {
  override def generate(payload: Latex): Try[Array[Byte]] = {
    val filename = generateTexFileName()
    writeTexToFile(payload, s"temp/sootsafe/$filename")
    LatexCompiler.latexToPdf(s"temp/sootsafe/$filename", "temp/sootsafe") match {
      case Success(pdfPath) =>
        Try(readPdf(pdfPath))
      case Failure(e) =>
        throw new Exception(s"Could not generate local PDF. Error: ${e.getMessage}")
    }
  }

  private def generateTexFileName(): String = {
    val format = new SimpleDateFormat("YYYY-MM-dd_HHmmss")
    format.format(new Date()) + ".tex"
  }

  private def writeTexToFile(tex: Latex, targetFilename: String): Unit = {
    val writer = new PrintWriter(new File(targetFilename))
    writer.write(tex)
    writer.close()
  }

  private def readPdf(filename: String): Array[Byte] = {
    Files.readAllBytes(Paths.get(filename))
  }

}