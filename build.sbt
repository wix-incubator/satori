organization := "com.wix"

name := "satori"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.1.1.201511131810-r",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "com.github.tototoshi" %% "scala-csv" % "1.2.2"
)
