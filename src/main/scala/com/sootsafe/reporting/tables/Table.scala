package com.sootsafe.reporting.tables

import java.util.UUID

import com.sootsafe.reporting.Texifyable

trait Table extends Texifyable {
  val identifier: UUID
}
