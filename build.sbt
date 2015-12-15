scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.1.1.201511131810-r",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "org.slf4j" % "slf4j-simple" % "1.7.9"
)
