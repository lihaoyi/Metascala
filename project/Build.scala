import org.scalajs.sbtplugin.cross.CrossProject
import sbt._
import Keys._

import sbtassembly.AssemblyPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Build extends sbt.Build {

  lazy val proj = Project(
    "SVM",
    file("."),
    settings =
      Defaults.coreDefaultSettings ++ Seq(
        organization := "com.example",
        version := "0.1",
        scalaVersion := "2.10.6",

        libraryDependencies ++= Seq(
          "org.ow2.asm" % "asm-debug-all" % "5.0.4",
          "org.scalatest" %% "scalatest" % "2.2.5" % "test",
          "org.mozilla" % "rhino" % "1.7R4",
          "com.nativelibs4java" %% "scalaxy-loops" % "0.3.3" % "provided"
        )
      )
  )
}
