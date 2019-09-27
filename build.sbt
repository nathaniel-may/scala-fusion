scalaVersion := "2.13.1"
name := "fusion"
version := "0.1"

lazy val fusion = (project in file("."))
  .settings(
    scalacOptions ++= Seq(
      "-language:postfixOps", 
      "-unchecked", 
      "-deprecation", 
      "-feature"),
      
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  )
