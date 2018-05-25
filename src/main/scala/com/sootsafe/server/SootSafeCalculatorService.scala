package com.sootsafe.server

import com.google.cloud.firestore.DocumentReference
import com.sootsafe.firebase.subscriber.{DefaultSubscriber, MessageSerializer}
import com.sootsafe.reporting.PdfGeneratorServiceClient
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass.ReleaseRateRequest
import com.sootsafe.server.requesthandler.ReleaseRateRequestHandler
import com.typesafe.config.{Config, ConfigFactory}
import io.grpc.{Server, ServerBuilder}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Channel, Future}

class SootSafeCalculatorService(port: Int) {

  private val config: Config = ConfigFactory.load()

  val pdfServiceHost: String = config.getString("pdfGenerator.address")
  val pdfServicePort: Int = config.getInt("pdfGenerator.port")

  val pdfGeneratorServiceClient = new PdfGeneratorServiceClient(pdfServiceHost, pdfServicePort)

  private val server: Server = ServerBuilder
    .forPort(port)
    .addService(new SootSafeCalculatorImpl())
    .addService(new ReleaseRateCalculatorImpl(pdfGeneratorServiceClient))
    .build()

  def start(): Unit = {
    server.start()
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        System.err.println("### gRPC Server shutting down since JVM is shutting down ###")
        SootSafeCalculatorService.this.stop()
        System.err.println("### Server shut down ###")
      }
    })
  }

  def stop(): Unit = {
    server.shutdown()
  }

  def blockUntilShutDown(): Unit = {
    if (server != null) {
      server.awaitTermination()
    }
  }
}

object Runner {

  private val config: Config = ConfigFactory.load()
  private val pdfServiceHost: String = config.getString("pdfGenerator.address")
  private val pdfServicePort: Int = config.getInt("pdfGenerator.port")

  def main(args: Array[String]): Unit = {

    val pdfGeneratorServiceClient = new PdfGeneratorServiceClient(pdfServiceHost, pdfServicePort)
    val calculatorService = new SootSafeCalculatorService(8980)
    calculatorService.start()

    val messageChannel = new Channel[(ReleaseRateRequest, DocumentReference)]

    val firestore = DefaultSubscriber.database()
    DefaultSubscriber.subscribe(firestore, referenceToRequest, messageChannel)

    Future {
      while (true) {
        val (releaseRateRequest, documentReference) = messageChannel.read
        ReleaseRateRequestHandler.handleRequest(releaseRateRequest, documentReference, pdfGeneratorServiceClient)
      }
    }

    calculatorService.blockUntilShutDown()
  }

  private val referenceToRequest: PartialFunction[String, ReleaseRateRequest] = {
    case stringRepr: String =>
      val builder = ReleaseRateCalculatorOuterClass.ReleaseRateRequest.newBuilder
      MessageSerializer.serializer[ReleaseRateRequest](stringRepr, builder)
  }
}