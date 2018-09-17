package com.sootsafe.server.requesthandler

import com.sootsafe.engine.zone.AtexCalculator
import com.sootsafe.server.calculator.AtexCalculator.{AtexCalculationResult, AtexRequest}


object AtexRequestHandler {
  def handleRequest(atexRequest: AtexRequest): Either[AtexCalculationResult, String] = {

    AtexCalculator.handleRequest(atexRequest) match {
      case Left(result) =>
        Left(result)
      case Right(errorStr) =>
        val errorMsg = s"Release Rate Error: $errorStr"
        println(errorMsg)
        Right(errorMsg)
    }
  }

}
