name := "cpumemory"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.9" % Test

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered := false