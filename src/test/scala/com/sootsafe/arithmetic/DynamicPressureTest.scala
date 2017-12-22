package com.sootsafe.arithmetic

import com.sootsafe.model
import com.sootsafe.model.{Dimension, Pipe}
import org.scalatest.{Matchers, WordSpecLike}

class DynamicPressureTest extends WordSpecLike with Matchers {

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
      vaporisation.texifyFormula() should be("""W_e = \dfrac{6,55\ u_w^{0,78}\ A_p\ p_V\ M^{0,667}}{R\ T} (kg/s)""")
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
      vmVaporisation.texifyFormula() should be("""Q_g \approx \dfrac{6,5\ u_w^{0,78}\ A_p\ p_V}{10^5\ M^{0,333}} \times \dfrac {T_a}{T} (m^3/s)""")
    }

    "texify calculation" in {
      vmVaporisation.texify() should be("""\dfrac{6.5 \times {1.0}^{0.78} \times 2.0 \times 1.15}{{10.0}^{5.0} \times {88.0}^{0.333}} \times \dfrac{992233.0}{9.09}""")
    }
  }

}
