package com.sootsafe.server

import java.util.Date

import com.sootsafe.engine.{Boverket, FlowAndPressureSequence}
import com.sootsafe.model.{Model, ModelBuilder}
import com.sootsafe.serializers.GRPCSerializer
import com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.{ErrorMessage, FirePressureCalculationResult}
import com.sootsafe.server.calculator.{SootSafeCalculatorGrpc, SootSafeCalculatorOuterClass}
import com.sootsafe.valuetable.RealValueResolver
import io.grpc.stub.StreamObserver
import io.grpc.{Server, ServerBuilder}

import scala.collection.JavaConversions._

class CalculatorImpl extends SootSafeCalculatorGrpc.SootSafeCalculatorImplBase {
  override def getFirePressure(request: SootSafeCalculatorOuterClass.SootSafeModel, responseObserver: StreamObserver[SootSafeCalculatorOuterClass.FirePressureCalculationResult]): Unit = {
    val now = new Date().getTime
    val nodes = request.getNodesList.map (GRPCSerializer.deserialize)
    val links = request.getLinksList.map(GRPCSerializer.deserialize)
    val model = Model(nodes.toList, links.toList)

    val reply = new ModelBuilder(model).buildModel() match {
      case Right(errorMessage) =>
        val errorResponse = ErrorMessage.newBuilder().setErrorCode(400).setErrorMessage(errorMessage)
        FirePressureCalculationResult.newBuilder().setErrorMessage(errorResponse).build()

      case Left(linkedNode) =>
        Boverket.calculatePressureLoss(linkedNode, initialFirePressure = request.getTargetFirePressure, valueResolver = RealValueResolver) match {
          case Right(errorMessage) =>
            val errorResponse = ErrorMessage.newBuilder().setErrorCode(400).setErrorMessage(errorMessage)
            FirePressureCalculationResult.newBuilder().setErrorMessage(errorResponse).build()
          case Left(result) =>
            FirePressureCalculationResult.newBuilder().addAllEntries(FlowAndPressureSequence.toEntries(result.seq)).build()
        }
    }

    responseObserver.onNext(reply)
    responseObserver.onCompleted()
    println(s"Time passed: ${new Date().getTime - now}")
  }
}

class SootSafeCalculatorService(port: Int) {

  private val server: Server = ServerBuilder
    .forPort(port)
    .addService(new CalculatorImpl())
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
    calculatorService.blockUntilShutDown()
  }
}