val modules = file("modules")

publish / skip := true

val testDependencies = libraryDependencies ++= Vector(
  "org.scalactic" %% "scalactic" % Version.scalaTest,
  "org.scalatest" %% "scalatest" % Version.scalaTest % Test,
  "org.scalameta" %% "munit"     % Version.munit     % Test
)

val parseDependencies2 = libraryDependencies ++= Vector(
)

val scala3Settings = scalacOptions ++= Vector(
  "-Yexplicit-nulls",
  "-Yshow-suppressed-errors",
  "-encoding",
  "utf-8",
)

val defaultSettings = testDependencies ++ scala3Settings

lazy val fix   = project
  .in(modules / "fix")
  .settings(name := "cherry-fix")

lazy val lamr  = project
  .in(modules / "lamr")
  .settings(name := "cherry-lamr")
  .dependsOn(fix)
  .settings(defaultSettings)

lazy val parse = project
  .in(modules / "parse")
  .settings(name := "cherry-parse")
  .settings(defaultSettings)
  .settings(libraryDependencies += "org.typelevel" %% "cats-parse" % Version.catsParse)
  .dependsOn(lamr)

lazy val tests = project
  .in(modules / "tests")
  .settings(publish / skip := true)
  .settings(defaultSettings)
  .dependsOn(lamr, parse)

lazy val adapt = project
  .in(modules / "adapt")
  .settings(name := "cherry-adapt")
  .settings(defaultSettings)
  .dependsOn(lamr, parse)
