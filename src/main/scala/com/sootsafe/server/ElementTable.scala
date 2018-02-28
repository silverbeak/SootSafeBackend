package com.sootsafe.server

case class ExplosionLimit(LFL: Double, UFL: Double)

case class Element(name: String, casNumber: String, egNumber: String, RDT: Double, flamePoint: Option[Double], volumeExplosionLimit: ExplosionLimit, weightExplosionLimit: ExplosionLimit, autoIgnitionTemperature: Double, temperatureClass: String, MESG: Double, explosionClass: String, gasDensity: Option[Double], molarMass: Double)

object ElementTable {

  val elements: Map[String, Element] = Map(
    "1333-74-0" -> Element("Hydrogen", casNumber = "", egNumber = "215-605-7", RDT = 0.07, flamePoint = None, volumeExplosionLimit = ExplosionLimit(LFL = 4.0, UFL = 77.0), weightExplosionLimit = ExplosionLimit(LFL = 3.4, UFL = 63), autoIgnitionTemperature = 538, temperatureClass = "T1", MESG = 0.29, explosionClass = "IIC", gasDensity = Some(0.0899), molarMass = 2.014 / 1000),
    "74-86-2" -> Element("Acetylene (ethyne)", casNumber = "", egNumber = "200-816-9", RDT = 0.90, flamePoint = None, volumeExplosionLimit = ExplosionLimit(LFL = 2.3, UFL = 100), weightExplosionLimit = ExplosionLimit(LFL = 24, UFL = 1092), autoIgnitionTemperature = 305, temperatureClass = "T2", MESG = 0.37, explosionClass = "IIC", gasDensity = Some(1.092), molarMass = 26.04 / 1000),
    "64-17-5" -> Element("Ethanol", casNumber = "", egNumber = "200-578-6", RDT = 1.59, flamePoint = Some(12), volumeExplosionLimit = ExplosionLimit(LFL = 3.1, UFL = 19.0), weightExplosionLimit = ExplosionLimit(LFL = 59, UFL = 532), autoIgnitionTemperature = 400, temperatureClass = "T2", MESG = 0.89, explosionClass = "IIB", gasDensity = Some(0), molarMass = 46.07 / 1000)
  )
}
