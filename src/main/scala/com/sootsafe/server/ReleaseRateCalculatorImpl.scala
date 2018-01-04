package com.sootsafe.server

import java.util.Date

import com.sootsafe.arithmetic._
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

object ReleaseRateCalculator extends Symbols {
  private[server] def handleRequest(request: ReleaseRateRequest): Either[ReleaseRateCalculationResult, String] = {


    Right("Not yet implemented")
  }

  private[server] def performCalculation(performReleaseCalculation: Boolean,
                                         hasReleaseRateInKgPerSecond: Boolean,
                                         isGasCalculation: Boolean,
                                         isEvaporationFromPool: Boolean,
                                         qg: Double,
                                         k: Double = .25,
                                         lfl: Double,
                                         wg: Double,
                                         M: Option[Double],
                                         rhoG: Option[Double]): Expression = {
    val (kSymbol, lflSymbol, qgSymbol) = prepareSymbols(performReleaseCalculation,
      hasReleaseRateInKgPerSecond,
      isGasCalculation,
      isEvaporationFromPool,
      qg, k, lfl, wg, M, rhoG)

    val formula = new ReleaseCharacter(kSymbol, lflSymbol, qgSymbol)

    formula
  }

  private[server] def prepareSymbols(performReleaseCalculation: Boolean,
                                     isGasCalculation: Boolean,
                                     hasReleaseRateInKgPerSecond: Boolean,
                                     isEvaporationFromPool: Boolean,
                                     qg: Double,
                                     k: Double,
                                     lfl: Double,
                                     wg: Double,
                                     M: Option[Double],
                                     rhoG: Option[Double]): (Symbol, Symbol, Symbol) = {

    (performReleaseCalculation, isGasCalculation, hasReleaseRateInKgPerSecond, isEvaporationFromPool) match {
      case (false, true, false, _) =>
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), Symbol(Value(qg), "Q_g"))

      case (false, false, false, _) =>
        // För vätska: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), Symbol(Value(qg), "Q_g"))

      case (false, true, true, _) =>
        // För gas: Beräkna Qg(ekv B.5)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        val b5formula = calculateB5(wg, M, rhoG)
        val qgSymbol = Symbol(Value(b5formula.calculate()), "Q_g")
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), qgSymbol)

      case (false, false, true, _) =>
        // För vätska: Beräkna Qg(ekv B.5)
        // För vätska: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        // TODO: Perform all calcs
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), Symbol(Value(qg), "Q_g"))

      case (true, false, _, false) =>
        // För gas: Beräkna Wg(ekv B.1)
        // För gas: Beräkna Qg(ekv B.5)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        // TODO: Perform all calcs
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), Symbol(Value(qg), "Q_g"))

      case (true, false, _, true) =>
        // För gas: Beräkna Wg(ekv B.6)
        // För gas: Beräkna Qg(ekv B.7)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        // TODO: Perform all calcs
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), Symbol(Value(qg), "Q_g"))

      case (true, true, _, _) =>
        // För vätska: Beräkna kritiskt tryck  (ekv B.2)
        // TODO: Perform all calcs
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), Symbol(Value(qg), "Q_g"))
    }
  }

  private[server] def calculateB5(wg: Double, M: Option[Double], rhoG: Option[Double]): Formula = {

    val rhoSymbol = (M, rhoG) match {
      case (_, Some(rho_g)) =>
        rho.copy(expression = Value(rho_g))

      case (Some(m), _) =>
        // First, calculate rho based on M
        val mSymbol = Symbol(Value(m), "M")
        val paSymbol = Symbol(Value(101325), "p_a")
        val RSymbol = Symbol(Value(8314), "R")
        val TaSymbol = Symbol(Value(393), "T_a")
        val calculatedRho = new MolarMassToRho(mSymbol, paSymbol, RSymbol, TaSymbol).calculate()

        rho.copy(expression = Value(calculatedRho))

      case _ => throw new Exception("Rho or M must be specified")
    }

    val wgSymbol = Symbol(Value(wg), "W_g")
    new VolumetricGasFlow(wgSymbol, rhoSymbol)
  }

}