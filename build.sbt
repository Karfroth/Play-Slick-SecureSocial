name := """play-slick-securesocial"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  "ws.securesocial" %% "securesocial" % "3.0-M7",
  "mysql" % "mysql-connector-java" % "5.1.40",
  "com.typesafe.play" %% "play-slick" % "2.0.2",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.0.2",
  "com.typesafe.slick" %% "slick" % "3.1.1",
  "com.github.tototoshi" %% "slick-joda-mapper" % "2.2.0",
  "joda-time" % "joda-time" % "2.7",
  "org.joda" % "joda-convert" % "1.7"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

// For eclipse
// EclipseKeys.preTasks := Seq(compile in Compile) 

fork in run := true