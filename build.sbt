ThisBuild / scalaVersion := "3.1.1"

val modules = file("modules")

publish / skip := true

val publishSettings = Vector(
  organization := "ru.tinkoff",
  version      := "0.0.1-Check4-SNAPSHOT",
)

val testDependencies = libraryDependencies ++= Vector(
  "org.scalactic" %% "scalactic" % Version.scalaTest % Test,
  "org.scalatest" %% "scalatest" % Version.scalaTest % Test,
)

val dependencies2 = Vector(
  "tf.tofu"       %% "tofu-kernel" % Version.tofu,
  "org.typelevel" %% "cats-free"   % Version.cats,
).map(_.cross(CrossVersion.for3Use2_13))

val coreDependencies = libraryDependencies ++= dependencies2

lazy val lamr = project
  .in(modules / "lamr")
  .settings(name := "cherry-lamr")
  .settings(publishSettings)
  .settings(coreDependencies)
  .settings(testDependencies)
