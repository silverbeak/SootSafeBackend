package com.sootsafe.server

import io.grpc.{Server, ServerBuilder}

class SootSafeCalculatorService(port: Int) {

  private val server: Server = ServerBuilder
    .forPort(port)
    .addService(new SootSafeCalculatorImpl())
    .build()

  def start(): Unit = {
    server.start()
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        System.err.println("### gRPC Server shutting down since JVM is shutting down ###")
        SootSafeCalculatorService.this.stop()
        System.err.println("### Server shut down ###")
      }
    })
  }

  def stop(): Unit = {
    server.shutdown()
  }

  def blockUntilShutDown(): Unit = {
    if (server != null) {
      server.awaitTermination()
    }
  }
}


object Runner {
  def main(args: Array[String]): Unit = {
    val calculatorService = new SootSafeCalculatorService(8980)
    calculatorService.start()
    calculatorService.blockUntilShutDown()
  }
}