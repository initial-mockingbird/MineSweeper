ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "Delilah"
  )
// Cats for functional programming
libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.0"

// circe for json stuff
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-yaml",
).map(_ % circeVersion)

// scalaTest for testing
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.16" % "test"

// kittens for that sweet sweet deriving
libraryDependencies += "org.typelevel" %% "kittens" % "3.0.0"
