package com.sootsafe.arithmetic

import com.sootsafe.model
import com.sootsafe.model.{Dimension, Pipe}
import org.scalatest.{Matchers, WordSpecLike}

class DynamicPressureTest extends WordSpecLike with Matchers with Symbols {

  val node = Pipe(0, model.SootSafeInfo("pipe", Some(33), None, None, None, Dimension(Some(2200), Some(125))))
  val dp = new DynamicPressure(node)

  "DynamicPressure" must {
    "texify formula" in {
      dp.texifyFormula() should be("""\zeta = \dfrac{ \pi^{2}d^{4} \Delta p_j } { 8 \rho q^{2} }""")
    }

    "texify calculation" in {
      dp.texify() should be("""\dfrac{8.0 \times 1.2 \times {0.033}^{2.0}}{{3.141592653589793}^{2.0} \times {0.125}^{4.0}}""")
    }
  }

  "Vaporisation" must {
    val uw: Symbol = Symbol(Value(1), "u_w")
    val ap: Symbol = Symbol(Value(2), """A_p""")
    val pv: Symbol = Symbol(Value(1.15), "p_V")
    val M: Symbol = Symbol(Value(88.0), "M")
    val R: Symbol = Symbol(Value(43), "R")
    val T: Symbol = Symbol(Value(09.09), "T")

    val vaporisation = new Evaporation(uw, ap, pv, M, R, T)

    "texify formula" in {
      vaporisation.texifyFormula() should be("""W_e = \dfrac{6,55\ u_w^{0,78}\ A_p\ p_V\ M^{0,667}}{R\ T}\ (kg/s)""")
    }

    "texify calculation" in {
      vaporisation.texify() should be("""\dfrac{6.55 \times {1.0}^{0.78} \times 2.0 \times 1.15 \times {88.0}^{0.667}}{43.0 \times 9.09}""")
    }
  }

  "Volumetric Vaporisation" must {
    val uw: Symbol = Symbol(Value(1), "u_w")
    val ap: Symbol = Symbol(Value(2), """A_p""")
    val pv: Symbol = Symbol(Value(1.15), "p_V")
    val M: Symbol = Symbol(Value(88.0), "M")
    val T: Symbol = Symbol(Value(09.09), "T")
    val Ta: Symbol = Symbol(Value(992233), "T_a")

    val vmVaporisation = new VolumetricEvaporation(uw, ap, pv, M, T, Ta)

    "texify formula" in {
      vmVaporisation.texifyFormula() should be("""Q_g \approx \dfrac{6,5\ u_w^{0,78}\ A_p\ p_V}{10^5\ M^{0,333}} \times \dfrac {T_a}{T}\ (m^3/s)""")
    }

    "texify calculation" in {
      vmVaporisation.texify() should be("""\dfrac{6.5 \times {1.0}^{0.78} \times 2.0 \times 1.15}{{10.0}^{5.0} \times {88.0}^{0.333}} \times \dfrac{992233.0}{9.09}""")
    }
  }

  "Release rate of liquids" must {

    val cd = Symbol(Value(33), "C_d")
    val s = Symbol(Division(Value(3), Value(4)), "S")
    val deltaP = Symbol(Value(55), """\Delta p""")

    val releaseRateOfLiquid = new ReleaseRateOfLiquid(cd, s, deltaP)

    "texify formula" in {
      releaseRateOfLiquid.texifyFormula() should be("""W = C_d\ S\ \sqrt{2\ \rho\ \Delta p}\ (kg/s)""")
    }

    "texify calculation" in {
      releaseRateOfLiquid.texify() should be("""33.0 \times \dfrac{3.0}{4.0} \times \sqrt{2.0 \times 1.2 \times 55.0}""")
    }
  }

  "Critical gas pressure" must {

    val pa = Symbol(Value(45), "p_a")
    val g = gamma.copy(expression = Value(33))

    val criticalGasPressure = new CriticalGasPressure(pa, g)

    "texify formula" in {
      criticalGasPressure.texifyFormula() should be("""p_C = p_a\ \left(\dfrac{  \gamma  + 1 }{2}\right)^{ \dfrac{  \gamma  }{  \gamma  - 1 }}\ (Pa)""")
    }

    "texify calculation" in {
      criticalGasPressure.texify() should be("""45.0 \times {\left({\dfrac{33.0 + 1.0}{2.0}}\right)}^{\dfrac{33.0}{33.0 - 1.0}}""")
    }
  }

  "Non limited gas rate" must {

    val cd: Symbol = Symbol(Value(34d), "C_d")
    val sp: Symbol = Symbol(Value(45.5), "S")
    val M: Symbol = Symbol(Value(23.999), "M")
    val R: Symbol = Symbol(Value(0.44), "R")
    val T: Symbol = Symbol(Value(0), "T")
    val Z: Symbol = Symbol(Value(88.4), "Z")
    val gma: Symbol = gamma.copy(expression = Value(.065))
    val pa: Symbol = Symbol(Value(.55), "p_a")
    val p: Symbol = Symbol(Value(0.005), "p")

    val nonLimitedGasRate = new NonLimitedGasRate(cd, sp, M, R, T, Z, gma, pa, p)

    "texify formula" in {
      nonLimitedGasRate.texifyFormula() should be("""W_g = C_d S p \sqrt{ \dfrac{M}{ZRT} \dfrac{2  \gamma }{ \gamma  - 1} \left[1 - \left( { \dfrac{p_a}{p} } \right) ^ { \dfrac{ \gamma  - 1}{ \gamma } } \right] } \left( { \dfrac{p_a}{p} } \right) ^ { \dfrac{1}{ \gamma } }\ (kg/s)""")
    }

    "texify calculation" in {
      println(nonLimitedGasRate.texify())
    }
  }

  "Limited gas rate" must {

    val cd: Symbol = Symbol(Value(34d), "C_d")
    val sp: Symbol = Symbol(Value(45.5), "S")
    val M: Symbol = Symbol(Value(23.999), "M")
    val R: Symbol = Symbol(Value(0.44), "R")
    val T: Symbol = Symbol(Value(0), "T")
    val Z: Symbol = Symbol(Value(88.4), "Z")
    val gma: Symbol = gamma.copy(expression = Value(.065))
    val p: Symbol = Symbol(Value(0.005), "p")

    val limitedGasRate = new LimitedGasRate(cd, sp, M, R, T, Z, gma, p)

    "texify formula" in {
      limitedGasRate.texifyFormula() should be("""W_g = C_d S p \sqrt{  \gamma  \dfrac{M}{ZRT} \left( { \dfrac{2}{ \gamma  + 1} } \right) ^ { \dfrac{ \gamma  + 1}{ \gamma  - 1} } }\ (kg/s)""")
    }

    "texify calculation" in {
      limitedGasRate.texify() should be("""34.0 \times 45.5 \times 0.005 \times \sqrt{0.065 \times \dfrac{23.999}{88.4 \times 0.44 \times 0.0} \times {\left({\dfrac{2.0}{0.065 + 1.0}}\right)}^{\dfrac{0.065 + 1.0}{0.065 - 1.0}}}""")
    }
  }

  "Release character" must {

    val k = Symbol(Value(0.25), "k")
    val lfl = Symbol(Value(10), "LFL")
    val qg = Symbol(Value(33.4), "Q_g")

    val releaseCharacter = new ReleaseCharacter(k, lfl, qg)

    "texify formula" in {
      releaseCharacter.texifyFormula() should be("""Q_{gk} = \dfrac{Q_g}{k\ LFL} (m^3/s)""")
    }

    "texify calculation" in {
      releaseCharacter.texify() should be("""\dfrac{33.4}{0.25 \times 10.0}""")
    }
  }

  "Volumetric gas flow" must {

    val wg = Symbol(Value(4.5), "W_g")
    val rhoG = Symbol(Value(0.88), "\\rho_g")

    val volumetricGasFlow = new VolumetricGasFlow(wg, rhoG)

    "texify formula" in {
      volumetricGasFlow.texifyFormula() should be("""Q_g = \dfrac{W_g}{\rho_g} (m^3/s)""")
    }

    "texify calculation" in {
      println(volumetricGasFlow.texify())
    }
  }

  "Molar mass to rho" must {

    val M = Symbol(Value(43), "M")
    val pa = Symbol(Value(.98), "p_a")
    val R = Symbol(Value(0.1), "R")
    val Ta = Symbol(Value(5), "T_a")

    val molarMassToRho = new MolarMassToRho(M, pa, R, Ta)

    "texify formula" in {
      molarMassToRho.texifyFormula() should be("""\rho_g = \dfrac{p_aM}{RT_a} (kg/m^3)""")
    }

    "texify calculation" in {
      molarMassToRho.texify() should be("""\dfrac{0.98 \times 43.0}{0.1 \times 5.0}""")
    }
  }

  "Background concentration" must {

    val Qg = Symbol(Value(43), "Q_g")
    val f = Symbol(Value(.98), "f")
    val Q2 = Symbol(Value(0.1), "Q_2")

    val backgroundConcentration = new BackgroundConcentrationFormula(f, Qg, Q2)

    "texify formula" in {
      backgroundConcentration.texifyFormula() should be("""X_b = \dfrac{fQ_g}{Q_2} (vol/vol)""")
    }

    "texify calculation" in {
      backgroundConcentration.texify() should be("""\dfrac{0.98 \times 43.0}{0.1}""")
    }
  }

}
