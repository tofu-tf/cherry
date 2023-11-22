ThisBuild / scalaVersion := Version.scala

ThisBuild / crossScalaVersions := Vector(Version.scala)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")

ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("base")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))

ThisBuild / githubWorkflowBuildPreamble += WorkflowStep.Sbt(
  List("scalafmtCheckAll", "scalafmtSbtCheck"),
  name = Some("Check formatting")
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    name = Some("Publish artifacts"),
    env = Map(
      "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

ThisBuild / licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

ThisBuild / developers := List(
  Developer("KS2003", "Yana Karpysheva", "karpyshev03@mail.ru", url("https://github.com/KS2003")),
  Developer(
    "mikhailchuryakov",
    "Michail Churyakov",
    "m.churyakov@tinkof.ru",
    url("https://github.com/mikhailchuryakov")
  ),
  Developer("odomontois", "Oleg Nizhnik", "odomontois@gmail.com", url("https://github.com/odomontois")),
  Developer("road21", "Aleksey Troitskiy", "amtroitskiy@gmail,com", url("https://github.com/road21")),
  Developer("saladNights", "Andrey Tarasov", "a.e.tarasov@tinkoff.ru", url("https://github.com/saladNights")),
  Developer("skye17", "Anastasiya Ermolaeva", "a.ermolaeva@tinkoff.ru", url("https://github.com/skye17"))
)

ThisBuild / organization     := "tf.tofu"
ThisBuild / organizationName := "Tofu"

ThisBuild / homepage := Some(url("https://github.com/tf-tofu/cherry"))

ThisBuild / description := "Universal Data design and transform language core"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/tofu-tf/cherry"),
    "git@github.com:tofu-tf/cherry.git"
  )
)

ThisBuild / githubWorkflowEnv += "CI" -> "true"
