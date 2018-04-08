package com.sootsafe.server.requesthandler

import java.net.URL
import java.nio.file.{Files, Paths}
import java.util.Date
import java.util.concurrent.TimeUnit

import com.google.cloud.firestore.DocumentReference
import com.google.firebase.cloud.StorageClient
import com.sootsafe.engine.zone.ReleaseRateCalculator
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass.ReleaseRateRequest


object ReleaseRateRequestHandler {
  def handleRequest(releaseRateRequest: ReleaseRateRequest, documentReference: DocumentReference): Unit = {
    import scala.collection.JavaConverters._
    ReleaseRateCalculator.handleRequest(releaseRateRequest) match {
      case Left(result) =>
        println(s"Successful! $result")
        // TODO: Write successful result to Firebase

        // Write report to firebase bucket and update document with link (need cleanup)
        val fileContent = readPdf(result._2)
        val blobPath = writeFileToFirebaseStorage(fileContent).toString
        val firestore = documentReference.getFirestore
        firestore.document(documentReference.getPath).collection("report").add(Map[String, String]("reportPath" -> blobPath).asJava)

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

  private def readPdf(filename: String): Array[Byte] = {
    Files.readAllBytes(Paths.get(filename))
  }
}
