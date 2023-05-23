val Http4sVersion  = "0.21.0-M5"
val Specs2Version  = "4.8.0"
val LogbackVersion = "1.2.3"

organization := "chess"
name := "chess"
version := "1.0"
scalaVersion := "2.13.10"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
  "org.http4s"     %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s"     %% "http4s-dsl"          % Http4sVersion,
  "org.specs2"     %% "specs2-core"         % Specs2Version % "test",
  "ch.qos.logback" % "logback-classic"      % LogbackVersion
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Xfatal-warnings"
)