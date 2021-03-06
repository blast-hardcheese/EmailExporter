import AssemblyKeys._ // put this at the top of the file

version := "0.2"

scalaVersion := "2.10.0"

libraryDependencies += "com.sun.mail" % "javax.mail" % "1.5.0"

libraryDependencies += "joda-time" % "joda-time" % "2.2"

libraryDependencies += "org.joda" % "joda-convert" % "1.3.1"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.1.0"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

assemblySettings
