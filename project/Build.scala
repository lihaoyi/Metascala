import sbt._
import Keys._

object Build extends sbt.Build{

  lazy val proj = Project(
    "SVM",
    file("."),
    settings =
      Defaults.defaultSettings ++ Seq(
      organization  := "com.example",
      version       := "0.1",
      scalaVersion  := "2.10.0",

      addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0"),

      scalacOptions += "-P:continuations:enable",

      resolvers ++= Seq(
        "typesafe repo"      at "http://repo.typesafe.com/typesafe/releases/",
        "spray nightly"      at "http://nightlies.spray.io/"
      ),
      libraryDependencies ++= Seq(
        "com.typesafe.akka"       %%  "akka-actor"    % "2.1.0",
        "com.typesafe.akka"       %%  "akka-testkit"    % "2.1.0" % "test",
        "org.ow2.asm" % "asm-debug-all" % "4.1",
        "org.scalatest" 		   % "scalatest_2.10.0" % "2.0.M5" % "test"
      )
    )
  )
}