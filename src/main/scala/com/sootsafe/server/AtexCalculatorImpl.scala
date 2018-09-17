package com.sootsafe.server

import java.util.Date

import com.sootsafe.server.calculator.AtexCalculator.{AtexCalculationResult, AtexCalculatorGrpc, AtexRequest}
import com.sootsafe.server.calculator.SootSafeCommon.ErrorMessage
import com.sootsafe.server.requesthandler.AtexRequestHandler

import scala.concurrent.Future

class AtexCalculatorImpl() extends AtexCalculatorGrpc.AtexCalculator {

  override def calculateAtex(request: AtexRequest): Future[AtexCalculationResult] = {
    val now = new Date().getTime

    val response = AtexRequestHandler.handleRequest(request) match {
      case Left(result) =>
        result
      case Right(error) =>
        val errorMessage = ErrorMessage(
          errorCode = 500,
          errorMessage = error
        )

        AtexCalculationResult(
          errorMessage = Some(errorMessage)
        )
    }

    println(s"Time passed: ${new Date().getTime - now}")
    Future.successful(response)
  }

}
