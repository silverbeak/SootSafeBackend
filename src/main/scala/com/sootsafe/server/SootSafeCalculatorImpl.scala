package com.sootsafe.server

import java.util.Date

import com.sootsafe.engine.{EngineProxy, FlowAndPressureSequence, PressureLossEngine}
import com.sootsafe.model.{LinkedNode, Model, ModelBuilder}
import com.sootsafe.serializers.GRPCSerializer
import com.sootsafe.server.calculator.SootSafeCalculatorOuterClass.FirePressureCalculationResult
import com.sootsafe.server.calculator.SootSafeCommon.ErrorMessage
import com.sootsafe.server.calculator.{SootSafeCalculatorGrpc, SootSafeCalculatorOuterClass}
import com.sootsafe.valuetable.RealValueResolver
import io.grpc.stub.StreamObserver

class SootSafeCalculatorImpl extends SootSafeCalculatorGrpc.SootSafeCalculatorImplBase {

  import scala.collection.JavaConversions._

  override def getFirePressure(request: SootSafeCalculatorOuterClass.SootSafeModel,
                               responseObserver: StreamObserver[SootSafeCalculatorOuterClass.FirePressureCalculationResult]): Unit = {

    val now = new Date().getTime
    val model = extractModelFromRequest(request)
    val initialFirePressure = request.getTargetFirePressure

    val reply = buildReply(model, initialFirePressure)

    responseObserver.onNext(reply)
    responseObserver.onCompleted()
    println(s"Time passed: ${new Date().getTime - now}")
  }

  private def buildReply(model: Model, initialFirePressure: Double) = {
    new ModelBuilder(model).buildModel() match {
      case Right(errorMessage) =>
        val errorResponse = ErrorMessage.newBuilder().setErrorCode(400).setErrorMessage(errorMessage)
        FirePressureCalculationResult
          .newBuilder()
          .setErrorMessage(errorResponse)
          .build()

      case Left(linkedNode) =>
        EngineProxy.getEngine("") match {
          case Left(engine) =>
            calculateWithEngine(initialFirePressure, linkedNode, engine)

          case Right(errorMessage) =>
            val errorResponse = ErrorMessage
              .newBuilder()
              .setErrorCode(400)
              .setErrorMessage(errorMessage)

            FirePressureCalculationResult
              .newBuilder()
              .setErrorMessage(errorResponse)
              .build()
        }
    }
  }

  private def calculateWithEngine(initialFirePressure: Double, linkedNode: LinkedNode, engine: PressureLossEngine) = {
    engine.calculatePressureLoss(linkedNode, initialFirePressure = initialFirePressure, valueResolver = RealValueResolver) match {
      case Right(errorMessage) =>
        val errorResponse = ErrorMessage
          .newBuilder()
          .setErrorCode(400)
          .setErrorMessage(errorMessage)

        FirePressureCalculationResult
          .newBuilder()
          .setErrorMessage(errorResponse)
          .build()

      case Left(result) =>
        FirePressureCalculationResult
          .newBuilder()
          .addAllEntries(FlowAndPressureSequence.toEntries(result.seq))
          .build()
    }
  }

  private def extractModelFromRequest(request: SootSafeCalculatorOuterClass.SootSafeModel) = {
    val nodes = request.getNodesList.map(GRPCSerializer.deserialize)
    val links = request.getLinksList.map(GRPCSerializer.deserialize)
    Model(nodes.toList, links.toList)
  }
}