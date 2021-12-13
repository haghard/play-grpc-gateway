import sbt._

organization := "com.ncl"

name := "play-grpc-gateway"

version := "0.0.1"

scalaVersion := "2.12.15"

scalacOptions ++= List("-encoding", "utf8", "-deprecation", "-feature", "-unchecked")

enablePlugins(SbtTwirl)

scalafmtOnCompile := true

libraryDependencies ++= {
  Seq(
    //patched dependency with method options
    "com.lightbend.akka.grpc" %% "akka-grpc-codegen" % "2.1.2"
      from """file:///Users/haghard/.ivy2/local/com.lightbend.akka.grpc/akka-grpc-codegen_2.12/2.1.2/jars/akka-grpc-codegen_2.12.jar""",
  )
}

scalafmtOnCompile := true

addCommandAlias("c", "compile")