import sbt._
import Keys._

object MyBuild extends Build {
    lazy val project = Project("EmailExporter", file("."))
// GitHub dependency examples:
// https://github.com/typesafehub/sbteclipse/issues/48
// http://stackoverflow.com/questions/16197456/sbt-direct-git-source-dependency-not-fetching-transitive-library-dependencies
        .dependsOn(uri("https://github.com/blast-hardcheese/jrtf.git"))
}
