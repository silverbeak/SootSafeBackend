name := "sootsafe-backend"

val dockerPrefix = "eu.gcr.io/sootsafe-app-test"

enablePlugins(sbtdocker.DockerPlugin, JavaAppPackaging)

dockerfile in docker := {
  val appDir: File = stage.value
  val targetDir = "/app"

  new Dockerfile {
    from("openjdk:slim")
    maintainer("Kristofer Jarl <kristofer.jarl@trollmoj.com>")
    entryPoint(s"$targetDir/bin/${executableScriptName.value}")
    copy(appDir, targetDir, chown = "daemon:daemon")
  }

  //  FROM aglover/java8-pier
  //  MAINTAINER Kristofer Jarl <kristofer@sootsafe.com>
  //
  //  WORKDIR build
  //
  //  ADD build/distributions/sootsafe-backend.tar /
  //
  //  ENTRYPOINT ["/sootsafe-backend/bin/sootsafe-backend"]
  //  #, "-cp", ".", "-Dconfig.file=/application.conf"]

}

imageNames in docker := {
  ImageName(repository = s"$dockerPrefix/${name.value}", tag = Some(s"0.1.0")) :: Nil
}

libraryDependencies ++= Seq(
  // https://mvnrepository.com/artifact/org.json4s/json4s-native
  "org.json4s" %% "json4s-native" % "3.6.1",

  // https://mvnrepository.com/artifact/com.google.firebase/firebase-admin
  "com.google.firebase" % "firebase-admin" % "6.5.0",

  // https://mvnrepository.com/artifact/org.json4s/json4s-jackson
  "org.json4s" %% "json4s-jackson" % "3.6.1",

  // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0",

  // https://mvnrepository.com/artifact/com.typesafe/config
  "com.typesafe" % "config" % "1.3.3",
)

// Protobuf and gRPC dependencies
libraryDependencies ++= Seq(
  // https://mvnrepository.com/artifact/io.grpc/grpc-all
  "io.grpc" % "grpc-stub" % scalapb.compiler.Version.grpcJavaVersion,
  "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
  "io.grpc" % "grpc-protobuf" % scalapb.compiler.Version.grpcJavaVersion,

  // https://mvnrepository.com/artifact/com.google.protobuf/protoc
  "com.google.protobuf" % "protoc" % "3.6.1" pomOnly(),

  "com.thesamet.scalapb" %% "scalapb-json4s" % "0.7.0",
  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
)

// Test dependencies
libraryDependencies ++= Seq(
  // https://mvnrepository.com/artifact/org.pegdown/pegdown
  "org.pegdown" % "pegdown" % "1.6.0" % Test,

  // https://mvnrepository.com/artifact/org.scalatest/scalatest
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,

  // https://mvnrepository.com/artifact/org.mockito/mockito-core
  "org.mockito" % "mockito-core" % "2.22.0" % Test,

  // https://mvnrepository.com/artifact/org.junit.jupiter/junit-jupiter-api
  "org.junit.jupiter" % "junit-jupiter-api" % "5.3.1" % Test,

  // https://mvnrepository.com/artifact/org.scalacheck/scalacheck
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
)

PB.protoSources in Compile := Seq(sourceDirectory.value / "main" / "protobuf" / "SootSafeProto" / "src" / "proto" / "sootsafe")

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)