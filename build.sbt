
inThisBuild(List(
  organization := "io.get-coursier",
  homepage := Some(url("https://github.com/coursier/jvm-index")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "alexarchambault",
      "Alexandre Archambault",
      "",
      url("https://github.com/alexarchambault")
    )
  )
))

lazy val shared = Def.settings(
  Compile / resourceDirectory := baseDirectory.value / "resources",
  scalaVersion := "3.0.1",
  libraryDependencies ++= Seq (
    "com.softwaremill.sttp.client3" %% "core" % "3.3.14",
    "com.lihaoyi" %% "ujson" % "1.4.0",
  ),
  Compile / sources := Seq(baseDirectory.value / "generate-index.sc")
)

name := "jvm-index"
shared
