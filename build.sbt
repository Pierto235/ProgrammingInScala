import Dependencies._

ThisBuild / scalaVersion     := "2.12.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val AkkaVersion = "2.6.8"
val AkkaHttpVersion = "10.2.4"

lazy val root = (project in file("."))
  .settings(
    name := "ProgrammingInScala",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.7",
    libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.0",
    libraryDependencies +="com.lucidchart" %% "xtract" % "2.2.1",
    libraryDependencies +=   "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
    libraryDependencies +=   "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion

  )



// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
