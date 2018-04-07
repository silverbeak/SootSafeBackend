package com.sootsafe.reporting.figures

import java.util.UUID

import com.sootsafe.reporting.Texifyable

trait Figure extends Texifyable {
  val identifier: UUID
}
