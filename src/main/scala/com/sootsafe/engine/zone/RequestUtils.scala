package com.sootsafe.engine.zone

import com.sootsafe.arithmetic.{Expression, Value}

trait RequestUtils {
  val getValue: Double => Expression = value => Value(value)
}
