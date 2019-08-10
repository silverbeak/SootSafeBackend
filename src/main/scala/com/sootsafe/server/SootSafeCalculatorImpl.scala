package com.sootsafe.server

import java.util.Date

import com.sootsafe.engine.{EngineProxy, FlowAndPressureSequence, PressureLossEngine}
import com.sootsafe.model.{LinkedNode, Model, ModelBuilder}
import com.sootsafe.serializers.GRPCSerializer
import com.sootsafe.server.calculator.SootSafeCalculator.{FirePressureCalculationResult, SootSafeCalculatorGrpc, SootSafeModel}
import com.sootsafe.server.calculator.SootSafeCommon.ErrorMessage
import com.sootsafe.valuetable.SuppliedValueResolver

import scala.concurrent.Future

class SootSafeCalculatorImpl extends SootSafeCalculatorGrpc.SootSafeCalculator {


  override def getFirePressure(request: SootSafeModel): Future[FirePressureCalculationResult] = {
    println(s"Received request! FirePressure: ${request.targetFirePressure}")
    val now = new Date().getTime
    val model = extractModelFromRequest(request)
    val initialFirePressure = request.targetFirePressure

    val reply = buildReply(model, initialFirePressure)

    println(s"Time passed: ${new Date().getTime - now}")
    Future.successful(reply)
  }

  private def buildReply(model: Model, initialFirePressure: Double): FirePressureCalculationResult = {
    new ModelBuilder(model).buildModel() match {
      case Right(errorMessage) =>
        val errorResponse = new ErrorMessage(
          errorCode = 400,
          errorMessage = errorMessage
        )

        FirePressureCalculationResult(
          errorMessage = Some(errorResponse)
        )

      case Left(linkedNode) =>
        EngineProxy.getEngine("") match {
          case Left(engine) =>
            calculateWithEngine(initialFirePressure, linkedNode, engine)

          case Right(errorMessage) =>
            val errorResponse = ErrorMessage(
              errorCode = 400,
              errorMessage = errorMessage
            )

            FirePressureCalculationResult(
              errorMessage = Some(errorResponse)
            )
        }
    }
  }

  private def calculateWithEngine(initialFirePressure: Double, linkedNode: LinkedNode, engine: PressureLossEngine): FirePressureCalculationResult = {
    engine.calculatePressureLoss(linkedNode, initialFirePressure = initialFirePressure, valueResolver = SuppliedValueResolver) match {
      case Right(errorMessage) =>
        val errorResponse = new ErrorMessage(
          errorCode = 400,
          errorMessage = errorMessage

        )

        new FirePressureCalculationResult(
          errorMessage = Some(errorResponse)
        )

      case Left(result) =>
        val entries = FlowAndPressureSequence.toEntries(result.seq)
        FirePressureCalculationResult(entries = entries)
    }
  }

  private def extractModelFromRequest(request: SootSafeModel): Model = {
    val nodes = request.nodes.map(GRPCSerializer.deserialize)
    val links = request.links.map(GRPCSerializer.deserialize)
    Model(nodes.toList, links.toList)
  }
}