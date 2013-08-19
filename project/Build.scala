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

      resolvers ++= Seq(
        "typesafe repo"      at "http://repo.typesafe.com/typesafe/releases/",
        "spray nightly"      at "http://nightlies.spray.io/",
        Resolver.sonatypeRepo("snapshots")
      ),
      libraryDependencies ++= Seq(
        "org.ow2.asm" % "asm-debug-all" % "4.1",
        "org.scalatest" 		   % "scalatest_2.10.0" % "2.0.M5" % "test",
        "org.mozilla" % "rhino" % "1.7R4",
        "com.nativelibs4java" %% "scalaxy-loops" % "0.3-SNAPSHOT" % "provided"
      )
    )
  )
}