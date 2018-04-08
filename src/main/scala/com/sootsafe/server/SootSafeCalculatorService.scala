package com.sootsafe.server

import com.google.cloud.firestore.DocumentReference
import com.google.protobuf.InvalidProtocolBufferException
import com.sootsafe.firebase.subscriber.{MessageSerializer, Subscriber}
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass.ReleaseRateRequest
import com.sootsafe.server.requesthandler.ReleaseRateRequestHandler
import io.grpc.{Server, ServerBuilder}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Channel, Future}

class SootSafeCalculatorService(port: Int) {

  private val server: Server = ServerBuilder
    .forPort(port)
    .addService(new SootSafeCalculatorImpl())
    .addService(new ReleaseRateCalculatorImpl())
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

  def main(args: Array[String]): Unit = {
    val calculatorService = new SootSafeCalculatorService(8980)
    calculatorService.start()

    val messageChannel = new Channel[(String, DocumentReference)]

    val firestore = Subscriber.database()
    Subscriber.subscribe(firestore, messageChannel)

    Future {
      while (true) {
        val changeMap = messageChannel.read
        //        println(s"Message: $changeMap")
        try {
          val builder = ReleaseRateCalculatorOuterClass.ReleaseRateRequest.newBuilder
          val releaseRateRequest = MessageSerializer.serializer[ReleaseRateRequest](changeMap._1, builder)
          ReleaseRateRequestHandler.handleRequest(releaseRateRequest, changeMap._2)
        } catch {
          case e: InvalidProtocolBufferException => println(s"Invalid protocol buffer exception! ${e.getMessage}")
          case e: Throwable =>
            //            println(s"Error caught[${e.getClass.getName}]: ${e.getMessage}")
            println(s"Error: ${e.getMessage}\n${e.getStackTrace.mkString("\n")}")
            throw new Exception(s"ERROR in channel!", e)
        }
      }
    }

    calculatorService.blockUntilShutDown()
  }
}