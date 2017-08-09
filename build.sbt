organization  := "com.example"
version       := "0.1"
scalaVersion  := "2.11.11"

libraryDependencies ++= Seq(
  "org.ow2.asm" % "asm-debug-all" % "5.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.mozilla" % "rhino" % "1.7R4"
)
