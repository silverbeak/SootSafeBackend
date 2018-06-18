package com.sootsafe.valuetable

import com.sootsafe.arithmetic.{Power, Value}
import com.sootsafe.calcutils.VelocityCalculator
import com.sootsafe.model._

trait PressureLossConstants {
  val rho: Double = 1.2
  val lambda: Double = 0.0216
}

trait ValueResolver {
  def ductPressureLoss(pipe: Pipe): Double

  def bendPressureLoss(bend: Bend): Double

  def areaIncrementPressureLoss(areaIncrement: AreaIncrement, originNode: NodeModule): Double

  def tPipePressureLoss(tPipe: TPipe, originNode: NodeModule): Double
}

object FakeValueResolver extends ValueResolver with PressureLossConstants {
  override def ductPressureLoss(pipe: Pipe): Double = {
    val R = pipe.ssInfo.nodeType.toLowerCase match {
      case "pipe" if pipe.ssInfo.dimension.diameter.contains(125) =>.8
      case "pipe" if pipe.ssInfo.capacity.contains(51) =>.6
      case "pipe" if pipe.ssInfo.capacity.contains(68) =>.9
      case "pipe" if pipe.ssInfo.capacity.contains(156) => 1.5
      //case "t-pipe" => .68
    }
    R * pipe.ssInfo.dimension.length.getOrElse(0d) / 1000
  }

  override def bendPressureLoss(bend: Bend): Double = {
    val zeta = VelocityCalculator.velocity(bend.ssInfo) match {
      case 0.003382042540702776 =>.35 // Fake first 90 degree bend
      case 0.004965634224467134 =>.3 // Fake second 90 degree bend
    }

    val v1 = VelocityCalculator.velocity(bend.ssInfo)
    rho * Math.pow(v1 * 1000, 2) / 2 * zeta
  }

  override def tPipePressureLoss(tPipe: TPipe, originNode: NodeModule): Double = {
    val v2 = Value(VelocityCalculator.velocity(originNode.ssInfo))
    val v1 = Value(VelocityCalculator.velocity(tPipe.ssInfo))

    // (Jensen 1990)
    val zeta = (Value(0.25) * Power(v2 / v1, Value(2)) - v2 / v1 + Value(1)).calculate()

    rho * Math.pow(v1.calculate() * 1000, 2) / 2 * zeta
  }

  override def areaIncrementPressureLoss(areaIncrement: AreaIncrement, originNode: NodeModule): Double = {
    val zeta = VelocityCalculator.velocity(areaIncrement.ssInfo) match {
      case 0.001691021270351388 => 0.14
      case 0.0021645072260497765 =>.12 // Fake second area increment
    }

    val v1 = VelocityCalculator.velocity(areaIncrement.ssInfo)
    rho * Math.pow(v1 * 1000, 2) / 2 * zeta
  }


}

object RealValueResolver extends ValueResolver with PressureLossConstants {

  /*
   * Taken from http://www.hvac.lth.se/fileadmin/hvac/TVIT-5031MMweb.pdf
   * Fix references and such later
   */
  private def dynamicPressure(nodeModule: NodeModule): Double = {
    val nominator = 8 * rho * Math.pow(nodeModule.ssInfo.capacity.get / 1000, 2)
    val denominator = Math.pow(Math.PI, 2) * Math.pow(nodeModule.ssInfo.dimension.diameter.get / 1000, 4)
    nominator / denominator
  }

  override def ductPressureLoss(pipe: Pipe): Double = {
    dynamicPressure(pipe) * (pipe.ssInfo.dimension.length.get / 1000) * lambda / (pipe.ssInfo.dimension.diameter.get / 1000)
  }

  /*
   * Resistance number is zeta in most tables and formulas
   */
  override def bendPressureLoss(bend: Bend): Double = {
    val resistanceNumber: Double = bend.ssInfo.dimension.diameter.getOrElse(0) match {
      case d: Double if d <= 20d => 1
      case d: Double if d <= 40d =>.5
      case d: Double if d <= 160d =>.35
      case _ =>.3
    }
    dynamicPressure(bend) * resistanceNumber
  }

  // TODO: Fix this... duh!
  override def tPipePressureLoss(tPipe: TPipe, originNode: NodeModule): Double = FakeValueResolver.tPipePressureLoss(tPipe, originNode)

  override def areaIncrementPressureLoss(areaIncrement: AreaIncrement, originNode: NodeModule): Double = FakeValueResolver.areaIncrementPressureLoss(areaIncrement, originNode)
}