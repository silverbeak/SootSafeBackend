package com.sootsafe.server

import java.util.Date

import com.sootsafe.server.calculator.AtexCalculatorGrpc
import com.sootsafe.server.calculator.AtexCalculatorOuterClass.{AtexCalculationResult, AtexRequest}
import com.sootsafe.server.calculator.SootSafeCommon.ErrorMessage
import com.sootsafe.server.requesthandler.AtexRequestHandler
import io.grpc.stub.StreamObserver

class AtexCalculatorImpl() extends AtexCalculatorGrpc.AtexCalculatorImplBase {

  override def calculateAtex(request: AtexRequest,
                              responseObserver: StreamObserver[AtexCalculationResult]): Unit = {
    val now = new Date().getTime

    val response = AtexRequestHandler.handleRequest(request) match {
      case Left(result) =>
        result
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
