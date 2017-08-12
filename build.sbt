organization  := "com.example"
version       := "0.1"
scalaVersion  := "2.11.11"

libraryDependencies ++= Seq(
  "org.ow2.asm" % "asm-debug-all" % "5.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.mozilla" % "rhino" % "1.7R4",
  "com.lihaoyi" %% "classparse" % "0.4.3" % "test",
  "com.lihaoyi" %% "fansi" % "0.2.4" % "test",
  "com.lihaoyi" %% "scalatags" % "0.6.5" % "test",
  "com.lihaoyi" %% "pprint" % "0.5.2" % "test",
  "com.google.errorprone" % "javac" % "9-dev-r4023-2"

)

libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.7" % "provided"

autoCompilerPlugins := true

//addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.7")
//
//scalacOptions += "-P:acyclic:force"