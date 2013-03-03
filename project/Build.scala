import sbt._

import Keys._
import AndroidKeys._

object General {
  val settings = Defaults.defaultSettings ++ Seq (
    name := "mcOptCal",
    version := "1.0",
    versionCode := 1,
    versionName := "1.01",
    scalaVersion := "2.9.2",
    platformName in Android := "android-15"
  )

  val proguardSettings = Seq (
    useProguard in Android := true
  )

  lazy val fullAndroidSettings =
    General.settings ++
    AndroidProject.androidSettings ++
    TypedResources.settings ++
    proguardSettings ++
    AndroidManifestGenerator.settings ++
    AndroidMarketPublish.settings ++ Seq (
      keyalias in Android := "dragongate_technologies",
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.8.RC1" % "test"
    )
}

object AndroidBuild extends Build {
  lazy val main = Project (
    "mcOptCal",
    file("."),
    settings = General.fullAndroidSettings
  )

  lazy val tests = Project (
    "tests",
    file("tests"),
    settings = General.settings ++
               AndroidTest.androidSettings ++
               General.proguardSettings ++ Seq (
      name := "Monte Carlo AppTests"
    )
  ) dependsOn main
}
