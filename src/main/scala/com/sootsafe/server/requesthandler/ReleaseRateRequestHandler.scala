package com.sootsafe.server.requesthandler

import java.net.URL
import java.util.Date
import java.util.concurrent.TimeUnit

import com.google.api.core.ApiFuture
import com.google.cloud.firestore.{DocumentReference, WriteResult}
import com.google.firebase.cloud.StorageClient
import com.sootsafe.engine.zone.ReleaseRateCalculator
import com.sootsafe.reporting.TexToPdfGenerator
import com.sootsafe.server.calculator.AtexCalculatorOuterClass.AtexRequest

import scala.util.{Failure, Try}


object ReleaseRateRequestHandler {
  def handleRequest(atexRequest: AtexRequest, documentReference: DocumentReference, pdfGenerator: TexToPdfGenerator): Try[ApiFuture[WriteResult]] = {
    import scala.collection.JavaConverters._

    ReleaseRateCalculator.handleRequest(atexRequest, pdfGenerator, generateReport = true) match {
      case Left(result) =>
        val blobPath = writeFileToFirebaseStorage(result._2).toString
        val firestore = documentReference.getFirestore
        Try(firestore.document(documentReference.getPath).collection("report").document("pdf").create(Map[String, String]("reportPath" -> blobPath).asJava))

      case Right(errorStr) =>
        val errorMsg = s"Release Rate Error: $errorStr"
        println(errorMsg)
        documentReference.getFirestore.collection("releaseRateErrors").add(Map[String, String](("message", errorMsg)))
        Failure(new Exception(errorMsg))
      // TODO: Write error to firebase
    }
  }

  private def writeFileToFirebaseStorage(payload: Array[Byte]): URL = {
    val storageRef = StorageClient.getInstance().bucket()
    val blob = storageRef.create(new Date().toString, payload, "application/pdf")
    blob.signUrl(24, TimeUnit.HOURS)
  }

}
