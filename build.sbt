name := "RepoSE"

version := "0.1"

fork in run := true

cancelable in Global := true

scalaVersion := "2.11.12"

resolvers += "uuverifiers" at "http://logicrunch.it.uu.se:4096/~wv/maven/"

libraryDependencies +=
  "uuverifiers" %% "princess-smt-parser" % "generalised-smt-parser-SNAPSHOT"

libraryDependencies +=
  "net.sf.squirrel-sql.thirdparty-non-maven" % "java-cup" % "0.11a"
