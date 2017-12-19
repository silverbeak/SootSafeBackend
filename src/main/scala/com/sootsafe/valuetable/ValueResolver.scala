package com.sootsafe.valuetable

import com.sootsafe.model.NodeModule

trait PressureLossConstants {
  val rho: Double = 1.2
}

trait ValueResolver {
  def ductPressureLoss(nodeModule: NodeModule): Double

  def componentPressureLoss(velocityFactor: Double): Double
}

object FakeValueResolver extends ValueResolver {
  override def ductPressureLoss(nodeModule: NodeModule): Double = {
    val R = nodeModule.ssInfo.nodeType.toLowerCase match {
      case "pipe" if nodeModule.ssInfo.dimension.diameter.contains(125) => .8
      case "pipe" if nodeModule.ssInfo.capacity.contains(51) =>.6
      case "pipe" if nodeModule.ssInfo.capacity.contains(68) =>.9
      case "pipe" if nodeModule.ssInfo.capacity.contains(156) => 1.5
      //case "t-pipe" => .68
    }
    R * nodeModule.ssInfo.dimension.length.getOrElse(0d) / 1000
  }

  override def componentPressureLoss(velocityFactor: Double): Double = {
    velocityFactor match {
      case 0.6103515625 => 0.14
      case 0.6666666666666666 => 0.4 //
      case 0.75 => 0.4 // Fake t-pipe
      case 3.382042540702776 =>.35 // Fake first 90 degree bend
      case 0.64 =>.12 // Fake second area increment
      case 0.4358974358974359 =>.6 // Fake (third) t-pipe
      case 4.965634224467134 =>.3 // Fake second 90 degree bend
    }
  }
}

object RealValueResolver extends ValueResolver with PressureLossConstants {

  /*
   * Taken from http://www.hvac.lth.se/fileadmin/hvac/TVIT-5031MMweb.pdf
   * Fix references and such later
   */
  override def ductPressureLoss(nodeModule: NodeModule): Double = {
    val nominator = 8 * 0.0216 * rho * nodeModule.ssInfo.dimension.length.get / 1000
    val denominator = Math.pow(Math.PI, 2) * Math.pow(nodeModule.ssInfo.dimension.diameter.get / 1000, 5)
    Math.pow(nodeModule.ssInfo.capacity.get / 1000, 2) * nominator / denominator
  }

  // TODO: Fix this... duh!
  override def componentPressureLoss(velocityFactor: Double): Double = FakeValueResolver.componentPressureLoss(velocityFactor)
}