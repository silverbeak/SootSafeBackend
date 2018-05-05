package com.sootsafe.server

case class ExplosionLimit(LFL: Double, UFL: Double)

case class Element(name: String, casNumber: String, egNumber: String, RDT: Double, flamePoint: Option[Double], volumeExplosionLimit: ExplosionLimit, weightExplosionLimit: ExplosionLimit, autoIgnitionTemperature: Double, temperatureClass: String, MESG: Double, explosionClass: String, gasDensity: Option[Double], molarMass: Double)

object ElementTable {

  val elements: Map[String, Element] = Map(
    "1333-74-0" -> Element("Hydrogen", casNumber = "", egNumber = "215-605-7", RDT = 0.07, flamePoint = None, volumeExplosionLimit = ExplosionLimit(LFL = .040, UFL = .77), weightExplosionLimit = ExplosionLimit(LFL = 3.4, UFL = 63), autoIgnitionTemperature = 538, temperatureClass = "T1", MESG = 0.29, explosionClass = "IIC", gasDensity = Some(0.0899), molarMass = 2.014 / 1000),
    "74-86-2" -> Element("Acetylene (ethyne)", casNumber = "", egNumber = "200-816-9", RDT = 0.90, flamePoint = None, volumeExplosionLimit = ExplosionLimit(LFL = .023, UFL = 1.00), weightExplosionLimit = ExplosionLimit(LFL = 24, UFL = 1092), autoIgnitionTemperature = 305, temperatureClass = "T2", MESG = 0.37, explosionClass = "IIC", gasDensity = Some(1.092), molarMass = 26.04 / 1000),
    "64-17-5" -> Element("Ethanol", casNumber = "", egNumber = "200-578-6", RDT = 1.59, flamePoint = Some(12), volumeExplosionLimit = ExplosionLimit(LFL = .031, UFL = .19), weightExplosionLimit = ExplosionLimit(LFL = 59, UFL = 532), autoIgnitionTemperature = 400, temperatureClass = "T2", MESG = 0.89, explosionClass = "IIB", gasDensity = Some(0), molarMass = 46.07 / 1000),
    "74-98-6" -> Element("Propane", casNumber = "74-98-6", egNumber = "200-827-9", RDT = 1.56, flamePoint = None, volumeExplosionLimit = ExplosionLimit(LFL = .017, UFL = .109), weightExplosionLimit = ExplosionLimit(LFL = 31, UFL = 200), autoIgnitionTemperature = 450, temperatureClass = "T2", MESG = 0.92, explosionClass = "IIA", gasDensity = Some(2.01), molarMass = 44.1 / 1000),
    "71-43-2" -> Element("Benzene", casNumber = "71-43-2", egNumber = "200-753-7", RDT = 2.70, flamePoint = Some(-11), volumeExplosionLimit = ExplosionLimit(LFL = .012, UFL = .086), weightExplosionLimit = ExplosionLimit(LFL = 39, UFL = 2809), autoIgnitionTemperature = 498, temperatureClass = "T1", MESG = 0.99, explosionClass = "IIA", gasDensity = Some(3.486), molarMass = 78.11 / 1000),
    "1330-20-7" -> Element("p-Xylene", casNumber = "1330-20-7", egNumber = "215-535-7", RDT = 3.66, flamePoint = Some(30), volumeExplosionLimit = ExplosionLimit(LFL = .010, UFL = .076), weightExplosionLimit = ExplosionLimit(LFL = 44, UFL = 335), autoIgnitionTemperature = 464, temperatureClass = "T1", MESG = 1.09, explosionClass = "IIA", gasDensity = Some(0.864), molarMass = 106.16 / 1000)
  )
}
