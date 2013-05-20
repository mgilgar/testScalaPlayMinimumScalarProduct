import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "testScalaPlayMinimumScalarProduct"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    
    // JSON LIB
    "net.sf.json-lib" % "json-lib" % "2.4" classifier "jdk15",
    "xom" % "xom" % "1.2.5",
    "org.mockito" % "mockito-all" % "1.9.5"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
