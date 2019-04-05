name := "Cross-compiled Morphology library"

crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.4")
scalaVersion := (crossScalaVersions in ThisBuild).value.last

// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val root = project.in(file(".")).
    aggregate(crossedJVM, crossedJS).
    settings(
      publish := {},
      publishLocal := {}
    )
    
val circeVersion = "0.11.1"


lazy val crossed = crossProject(JSPlatform, JVMPlatform).in(file("."))
.settings(
      name := "fumorph",
      organization := "edu.furman.classics",
      version := "0.9.0",
      licenses += ("GPL-3.0",url("https://opensource.org/licenses/gpl-3.0.html")),
      resolvers += Resolver.jcenterRepo,
      resolvers += Resolver.bintrayRepo("eumaeus", "maven"),
      resolvers += Resolver.bintrayRepo("neelsmith", "maven"),
      retrieveManaged := true,
      libraryDependencies ++= Seq(
        "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
        "org.scalatest" %%% "scalatest" % "3.0.1" % "test",
        "edu.holycross.shot.cite" %%% "xcite" % "4.0.2",
        "edu.holycross.shot" %%% "citeobj" % "7.3.0",
        "edu.holycross.shot" %%% "citerelations" % "2.4.0",
        "edu.holycross.shot" %%% "greek" % "2.3.2",
        "edu.holycross.shot" %%% "ohco2" % "10.12.5",
        "edu.holycross.shot" %%% "dse" % "3.1.0",
        "edu.holycross.shot" %%% "scm" % "6.2.0"
      ),
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser"
      ).map(_ % circeVersion)
    ).
    jvmSettings(
      tutTargetDirectory := file("docs"),
      tutSourceDirectory := file("shared/src/main/tut")
    ).
    jsSettings(
      skip in packageJSDependencies := false,
      scalaJSUseMainModuleInitializer in Compile := true
    )

lazy val crossedJVM = crossed.jvm.enablePlugins(TutPlugin)
lazy val crossedJS = crossed.js