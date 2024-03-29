scalaVersion := "2.13.0"
name := "fusion"
version := "0.1"

lazy val fusion = (project in file("."))
  .settings(
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-language:higherKinds", 
      "-unchecked", 
      "-deprecation", 
      "-feature"),

    libraryDependencies += "org.scalatest"  %% "scalatest"  % "3.0.8"  % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    libraryDependencies += "org.typelevel"  %% "cats-core"  % "2.0.0"  % "test",

    libraryDependencies += "org.scoverage" %% "scalac-scoverage-plugin" % "1.4.0" % "provided"
  )
