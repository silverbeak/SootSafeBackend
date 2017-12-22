package com.sootsafe.arithmetic

case class Symbol(expression: Expression, sign: String)

trait Units {
  val kg_per_second: String = "kg/s"

  val l_per_second: String = "l/s"

  val pascal: String = "Pa"
}

trait Symbols {
  val rho = Symbol(Value(1.2d), """ \rho """)

  val pi = Symbol(Value(Math.PI), """ \pi """)

  val delta = Symbol(Expression.Zero, """ \Delta """)
}
