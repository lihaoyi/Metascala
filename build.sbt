
organization  := "com.example"
version       := "0.1"
scalaVersion  := "2.11.11"

libraryDependencies ++= Seq(
  "org.ow2.asm" % "asm-debug-all" % "5.2",
  "com.lihaoyi" %% "utest" % "0.5.3" % "test",
  "org.mozilla" % "rhino" % "1.7.7.1" % "test",
  "com.lihaoyi" %% "classparse" % "0.4.3" % "test",
  "com.lihaoyi" %% "fansi" % "0.2.4",
  "com.lihaoyi" %% "scalatags" % "0.6.5" % "test",
  "com.lihaoyi" %% "pprint" % "0.5.2",
  "com.google.errorprone" % "javac" % "9-dev-r4023-2",
  "com.lihaoyi" % "ammonite" % "1.0.2" cross CrossVersion.full
)

logBuffered := false

testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.7" % "provided"

autoCompilerPlugins := true

addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.7")

scalacOptions += "-P:acyclic:force"

fork in run := true

connectInput := true

parallelExecution in Test := false
