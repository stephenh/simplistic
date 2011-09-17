  import sbt._
  
  import Keys._

  object BuildSettings {
    val buildOrganization = "simplistic"
    val buildScalaVersion = "2.9.0-1"
    val buildVersion      = "1.0.17-SNAPSHOT"
    
    val buildSettings = Defaults.defaultSettings ++ Seq (organization := buildOrganization,
						         scalaVersion := buildScalaVersion,
						         version      := buildVersion)
  }

  
  object Dependencies {

    val logbackVersion	= "0.9.16"
    
    val httpclient = "org.apache.httpcomponents" % "httpclient" % "4.1.1"
    // val slf4j      = "org.slf4j" % "slf4j" % "1.5.6"

    val logbackcore	= "ch.qos.logback" % "logback-core"     % logbackVersion
    val logbackclassic	= "ch.qos.logback" % "logback-classic"  % logbackVersion  

    val scalatest       = "org.scalatest"  % "scalatest_2.9.0"  % "1.4.1" % "test"
    
  }

  object StdCompilerOptions {
    val compilerOptions  = Seq ("-Xcheckinit", "-deprecation")
  }

  object SimplisticBuild extends Build {
    
    import BuildSettings._
    import Dependencies._
    import StdCompilerOptions._

    val deps = Seq (httpclient, logbackcore, scalatest)

    lazy val simplistic = Project ("simplistic", file ("."),
                                   settings = buildSettings ++ Seq (libraryDependencies := deps,
                                                                    scalacOptions := compilerOptions)) aggregate ()
    
  }


