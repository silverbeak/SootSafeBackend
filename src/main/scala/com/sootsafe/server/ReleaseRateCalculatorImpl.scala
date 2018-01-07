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
                                         Qg: Double,
                                         k: Double = .25,
                                         lfl: Double,
                                         Wg: Double,
                                         M: Option[Double],
                                         rhoG: Option[Double],
                                         Cd: Double,
                                         S: Double,
                                         deltaP: Double,
                                         Ap: Double,
                                         uw: Double,
                                         T: Double,
                                         gma: Double,
                                         pa: Double,
                                         p: Double,
                                         criticalGasPressure: Double,
                                         compressibilityFactor: Double): Expression = {

    val (kSymbol, lflSymbol, qgSymbol) = prepareSymbols(performReleaseCalculation,
      hasReleaseRateInKgPerSecond,
      isGasCalculation,
      isEvaporationFromPool,
      Qg, k, lfl, Wg, M, rhoG, Cd, S, deltaP, Ap, uw, T, gma, pa, p,
      criticalGasPressure,
      compressibilityFactor)

    val formula = new ReleaseCharacter(kSymbol, lflSymbol, qgSymbol)

    formula
  }

  private[server] def prepareSymbols(performReleaseCalculation: Boolean,
                                     isGasCalculation: Boolean,
                                     hasReleaseRateInKgPerSecond: Boolean,
                                     isEvaporationFromPool: Boolean,
                                     Qg: Double,
                                     k: Double,
                                     lfl: Double,
                                     Wg: Double,
                                     M: Option[Double],
                                     rhoG: Option[Double],
                                     Cd: Double,
                                     S: Double,
                                     deltaP: Double,
                                     Ap: Double,
                                     uw: Double,
                                     T: Double,
                                     gma: Double,
                                     pa: Double,
                                     p: Double,
                                     criticalPressure: Double,
                                     compressibilityFactor: Double): (Symbol, Symbol, Symbol) = {

    val R = 8324d
    val rSymbol = Symbol(Value(R), "R")

    (performReleaseCalculation, isGasCalculation, hasReleaseRateInKgPerSecond, isEvaporationFromPool) match {
      case (false, true, false, _) =>
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), Symbol(Value(Qg), "Q_g"))

      case (false, false, false, _) =>
        // För vätska: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), Symbol(Value(Qg), "Q_g"))

      case (false, _, true, _) =>
        // För gas: Beräkna Qg(ekv B.5)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)
        val b5formula = calculateB5(Wg, M, rhoG)
        val qgSymbol = Symbol(Value(b5formula.calculate()), "Q_g")
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), qgSymbol)

      case (true, false, _, false) =>
        // För gas: Beräkna Wg(ekv B.1)
        // För gas: Beräkna Qg(ekv B.5)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)

        val cdSymbol = Symbol(Value(Cd), "C_d")
        val sSymbol = Symbol(Value(S), "S")
        val deltaPSymbol = Symbol(Value(deltaP), s"${delta.sign}p")

        val wg = new ReleaseRateOfLiquid(cdSymbol, sSymbol, deltaPSymbol).calculate()

        val b5formula = calculateB5(wg, M, rhoG)
        val qgSymbol = Symbol(Value(b5formula.calculate()), "Q_g")
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), qgSymbol)

      case (true, false, _, true) =>
        // För gas: Beräkna Wg(ekv B.6)
        // För gas: Beräkna Qg(ekv B.7)
        // För gas: Beräkna utsläppets karaktär dvs Qg /(k*LFL)

        val uwSymbol = Symbol(Value(uw), "u_w")
        val apSymbol = Symbol(Value(Ap), "A_p")
        val pvSymbol = Symbol(Value(p), "p")
        val mSymbol = Symbol(Value(M.getOrElse(???)), "M")
        val tSymbol = Symbol(Value(T), "T")

        val we = new Evaporation(uwSymbol, apSymbol, pvSymbol, mSymbol, rSymbol, tSymbol).calculate()

        val b5formula = calculateB5(we, M, rhoG)
        val qgSymbol = Symbol(Value(b5formula.calculate()), "Q_g")
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), qgSymbol)

      case (true, true, _, _) =>
        // För vätska: Beräkna kritiskt tryck  (ekv B.2)

        val gmaSymbol = gamma.copy(expression = Value(gma))
        val paSymbol = Symbol(Value(pa), "p_a")
        val mSymbol = Symbol(Value(M.getOrElse(???)), "M")
        val tSymbol = Symbol(Value(T), "T")
        val cdSymbol = Symbol(Value(Cd), "C_d")
        val sSymbol = Symbol(Value(S), "S")
        val zSymbol = Symbol(Value(compressibilityFactor), "Z")
        val pSymbol = Symbol(Value(p), "p")

        val wg = new CriticalGasPressure(paSymbol, gmaSymbol).calculate() match {
          case gasPressure if gasPressure > criticalPressure =>
            // För vätska: Beräkna Wg genom B.3
            new NonLimitedGasRate(cdSymbol, sSymbol, mSymbol, rSymbol, tSymbol, zSymbol, gmaSymbol, paSymbol, pSymbol)
          case _ =>
            // För vätska: Beräkna Wg genom B.4
            new LimitedGasRate(cdSymbol, sSymbol, mSymbol, rSymbol, tSymbol, zSymbol, gmaSymbol, pSymbol)
        }

        val b5formula = calculateB5(wg.calculate(), M, rhoG)
        val qgSymbol = Symbol(Value(b5formula.calculate()), "Q_g")
        (Symbol(Value(k), "k"), Symbol(Value(lfl), "LFL"), qgSymbol)
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