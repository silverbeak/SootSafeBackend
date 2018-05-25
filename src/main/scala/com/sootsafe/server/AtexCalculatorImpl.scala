package com.sootsafe.server

import java.util.Date

import com.sootsafe.engine.zone.AtexCalculator
import com.sootsafe.reporting.TexToPdfGenerator
import com.sootsafe.server.calculator.AtexCalculatorGrpc
import com.sootsafe.server.calculator.AtexCalculatorOuterClass.{AtexCalculationResult, AtexRequest}
import com.sootsafe.server.calculator.SootSafeCommon.ErrorMessage
import io.grpc.stub.StreamObserver

class AtexCalculatorImpl(pdfGenerator: TexToPdfGenerator) extends AtexCalculatorGrpc.AtexCalculatorImplBase {

  override def calculateAtex(request: AtexRequest,
                              responseObserver: StreamObserver[AtexCalculationResult]): Unit = {
    val now = new Date().getTime

    val response = AtexCalculator.handleRequest(request, pdfGenerator) match {
      case Left(result) =>
        result._1
      case Right(error) =>
        val errorMessage = ErrorMessage
          .newBuilder()
          .setErrorCode(500)
          .setErrorMessage(error)
          .build()

        AtexCalculationResult
          .newBuilder()
          .setErrorMessage(errorMessage)
          .build()
    }

    responseObserver.onNext(response)
    responseObserver.onCompleted()
    println(s"Time passed: ${new Date().getTime - now}")
  }

}
