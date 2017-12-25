package com.sootsafe.engine

object EngineProxy {

  def getEngine(engineName: String): Either[PressureLossEngine, String] = engineName match {
    case "boverket" => Left(Boverket)
    case _ => Left(Boverket)
  }

}
