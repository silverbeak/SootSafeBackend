package com.sootsafe.server

import java.util.Date

import com.sootsafe.server.calculator.ReleaseRateCalculatorGrpc
import com.sootsafe.server.calculator.ReleaseRateCalculatorOuterClass.{ReleaseRateCalculationResult, ReleaseRateRequest}
import com.sootsafe.server.calculator.SootSafeCommon.ErrorMessage
import io.grpc.stub.StreamObserver

class ReleaseRateCalculatorImpl extends ReleaseRateCalculatorGrpc.ReleaseRateCalculatorImplBase {

  override def getReleaseRate(request: ReleaseRateRequest,
                              responseObserver: StreamObserver[ReleaseRateCalculationResult]): Unit = {
    val now = new Date().getTime

    val response = ReleaseRateCalculator.handleRequest(request) match {
      case Left(result) => result
      case Right(error) =>
        val errorMessage = ErrorMessage
          .newBuilder()
          .setErrorCode(500)
          .setErrorMessage(error)
          .build()

        ReleaseRateCalculationResult
          .newBuilder()
          .setErrorMessage(errorMessage)
          .build()
    }

    responseObserver.onNext(response)
    responseObserver.onCompleted()
    println(s"Time passed: ${new Date().getTime - now}")
  }

}
