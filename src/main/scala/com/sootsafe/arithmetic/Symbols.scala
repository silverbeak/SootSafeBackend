package com.sootsafe.arithmetic

case class Symbol(expression: Expression, sign: String)

trait Units {
  val kg_per_second: String = "kg/s"

  val l_per_second: String = "l/s"

  val pascal: String = "Pa"

  val cubic_meter_per_second: String = "m^3/s"

  val kg_per_cubic_meter: String = "kg/m^3"

  val vol_vol: String = "vol/vol"
}

trait Symbols {
  val rho = Symbol(Value(1.2d), """ \rho """)

  val pi = Symbol(Value(Math.PI), """ \pi """)

  val delta = Symbol(Expression.Zero, """ \Delta """)

  val gamma = Symbol(Expression.Zero, """ \gamma """)
}
