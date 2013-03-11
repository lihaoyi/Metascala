import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

object Build extends sbt.Build{

  lazy val proj = Project(
    "SVM",
    file("."),
    settings =
      Defaults.defaultSettings ++ assemblySettings ++ Seq(
      organization  := "com.example",
      version       := "0.1",
      scalaVersion  := "2.10.0",

      addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0"),

      scalacOptions += "-P:continuations:enable",

      test in assembly := {},

      resolvers ++= Seq(
        "typesafe repo"      at "http://repo.typesafe.com/typesafe/releases/",
        "spray nightly"      at "http://nightlies.spray.io/"
      ),
      libraryDependencies ++= Seq(
        "com.typesafe.akka"       %%  "akka-actor"    % "2.1.0",
        "com.typesafe.akka"       %%  "akka-testkit"    % "2.1.0" % "test",
        "rhino" % "js" % "1.7R2",
        "org.ow2.asm" % "asm-debug-all" % "4.1",
        "org.scalatest" 		   % "scalatest_2.10.0" % "2.0.M5" % "test",
        "com.fasterxml.jackson.core" % "jackson-databind" % "2.1.3"
      )
    )
  )
}