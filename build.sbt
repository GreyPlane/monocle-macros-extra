ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

ThisBuild / scalacOptions := Seq(
  "-Ymacro-debug-lite",
  "-Ymacro-annotations"
)

val MonocleVersion = "3.2.0"

lazy val macros = (project in file("macros")).settings(
  name := "monocle-macros-extra-macros",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % "2.13.12" % "provided",
    "dev.optics" %% "monocle-core" % MonocleVersion,
    "dev.optics" %% "monocle-macro" % MonocleVersion
  )
)

lazy val root = (project in file("."))
  .settings(
    name := "monocle-macros-extra",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core" % MonocleVersion,
      "dev.optics" %% "monocle-macro" % MonocleVersion,
      "org.scalatest" %% "scalatest" % "3.2.15" % Test
    )
  )
  .dependsOn(macros)
