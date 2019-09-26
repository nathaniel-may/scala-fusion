scalaVersion := "2.12.8"
name := "fusion"
version := "0.1"

lazy val fusion = (project in file("."))
  .settings(
    scalacOptions ++= Seq(
      "-language:postfixOps", 
      "-unchecked", 
      "-deprecation", 
      "-feature", 
      "-Xfuture"),
      
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  )
