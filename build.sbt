ThisBuild / scalaVersion := "3.1.0"

val modules = file("modules")

val testDependencies = libraryDependencies ++= Vector(
  "org.scalactic" %% "scalactic" % Version.scalaTest % Test,
  "org.scalatest" %% "scalatest" % Version.scalaTest % Test,
)

val dependencies2 = Vector(
  "tf.tofu"       %% "tofu-core-ce3" % Version.tofu,
  "org.typelevel" %% "cats-free"     % Version.cats,
).map(_.cross(CrossVersion.for3Use2_13))

val coreDependencies = libraryDependencies ++= Vector(
) ++ dependencies2

lazy val lamr        = project
  .in(modules / "lamr")
  .settings(coreDependencies)
  .settings(testDependencies)
