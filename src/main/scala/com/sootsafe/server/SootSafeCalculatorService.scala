package com.sootsafe.server

import com.google.cloud.firestore.DocumentReference
import com.sootsafe.firebase.subscriber.{DefaultSubscriber, MessageSerializer}
import com.sootsafe.reporting.PdfGeneratorServiceClient
import com.sootsafe.server.calculator.AtexCalculatorOuterClass
import com.sootsafe.server.calculator.AtexCalculatorOuterClass.AtexRequest
import com.sootsafe.server.requesthandler.AtexRequestHandler
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
    .addService(new AtexCalculatorImpl(pdfGeneratorServiceClient))
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
    val firestore = DefaultSubscriber.database()

    val atexMessageChannel = new Channel[(AtexRequest, DocumentReference)]
    DefaultSubscriber.subscribe("atexRequests", firestore, referenceToAtexRequest, atexMessageChannel)

    Future {
      while (true) {
        val (atexRequest, documentReference) = atexMessageChannel.read
        AtexRequestHandler.handleRequest(atexRequest, documentReference, pdfGeneratorServiceClient)
      }
    }

//    val fidMessageChannel = new Channel[(SootSafeModel, DocumentReference)]
//    DefaultSubscriber.subscribe("fidRequests", firestore, referenceToFidRequest, fidMessageChannel)
//
//    Future {
//      while (true) {
//        val (fidRequest, documentReferece) = fidMessageChannel.read
//
//      }
//    }

    calculatorService.blockUntilShutDown()
  }

  private val referenceToAtexRequest: PartialFunction[String, AtexRequest] = {
    case stringRepr: String =>
      val builder = AtexCalculatorOuterClass.AtexRequest.newBuilder
      MessageSerializer.serializer[AtexRequest](stringRepr, builder)
  }

//  private val referenceToFidRequest: PartialFunction[String, SootSafeModel] = {
//    case stringRepr =>
//      val builder = SootSafeCalculatorOuterClass.SootSafeModel.newBuilder
//      MessageSerializer.serializer[SootSafeModel](stringRepr, builder)
//  }
}