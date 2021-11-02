ThisBuild / scalaVersion := "3.1.0"

val modules = file("modules")

val testSettings = Vector(
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10" % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test
)

lazy val lamr    = project.in(modules / "lamr").settings(testSettings)
