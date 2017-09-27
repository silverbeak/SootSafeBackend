name := "SootsafeBackend"

version := "0.1"

scalaVersion := "2.11.8"

// https://mvnrepository.com/artifact/org.json4s/json4s-native_2.12
libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.5.3",

  // https://mvnrepository.com/artifact/org.scalatest/scalatest_2.11
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"

)
