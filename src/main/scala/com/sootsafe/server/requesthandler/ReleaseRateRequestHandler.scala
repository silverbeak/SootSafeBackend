package com.sootsafe.server.requesthandler

import java.net.URL
import java.util.Date
import java.util.concurrent.TimeUnit

import com.google.cloud.firestore.DocumentReference
import com.google.firebase.cloud.StorageClient
import com.sootsafe.engine.zone.ReleaseRateCalculator
import com.sootsafe.reporting.TexToPdfGenerator
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass.ReleaseRateRequest


object ReleaseRateRequestHandler {
  def handleRequest(releaseRateRequest: ReleaseRateRequest, documentReference: DocumentReference, pdfGenerator: TexToPdfGenerator): Unit = {
    import scala.collection.JavaConverters._

    ReleaseRateCalculator.handleRequest(releaseRateRequest, pdfGenerator, generateReport = true) match {
      case Left(result) =>
        val blobPath = writeFileToFirebaseStorage(result._2).toString
        val firestore = documentReference.getFirestore
        firestore.document(documentReference.getPath).collection("report").document("pdf").create(Map[String, String]("reportPath" -> blobPath).asJava)

      case Right(errorStr) =>
        println(s"Encountered error: $errorStr")
      // TODO: Write error to firebase
    }
  }

  private def writeFileToFirebaseStorage(payload: Array[Byte]): URL = {
    val storageRef = StorageClient.getInstance().bucket()
    val blob = storageRef.create(new Date().toString, payload, "application/pdf")
    blob.signUrl(24, TimeUnit.HOURS)
  }

}
