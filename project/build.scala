package tryp

import sbt._
import Keys._

import TrypBuildKeys._
import Templates.autoImport._

import coursier._

object PulsarKeys
{
  val dogsVersion = settingKey[String]("dogs version")
  val monocleVersion = settingKey[String]("monocle version")
}
import PulsarKeys._

object PulsarBuild
extends tryp.LibsBuild("pulsar", deps = PulsarDeps)
{
  val twelve = settingKey[Boolean]("use 2.12")

  override def defaultBuilder = { name: String =>
    super.defaultBuilder(name).settingsV(
      publishArtifact in (Compile, packageDoc) := false,
      // crossScalaVersions += "2.11.8",
      version := "0.2.0-SNAPSHOT"
    )
  }

  lazy val core = "core" / "Essential helpers"

  lazy val unitCore = "unit-core" ~ "unit/core" / "unit test core" << core

  lazy val macros = "macros" / "General macro helpers" << core

  lazy val main = "main" / "Core tryp functionality" << macros

  lazy val test = "test" / "Universal test helpers" << main

  lazy val unitMain = "unit-main" ~ "unit/main" / "Common helpers for unit tests" << unitCore << test

  lazy val app = "app" / "Macros for generating cross-platform app models" << main

  // lazy val slickMeta = "slick-meta" / "macros for slick-core" << app

  // lazy val slickCore = "slick-core" / "Model implementation for slick/sqlite" << slickMeta

  // lazy val slick = "slick" / "Slick extensions for rest backend sync" << slickCore << test

  def u(name: String) =
    name
    .logback("tag" -> "tryp")
    .settingsV(
      generateLogback := false,
      (logbackTemplate in Test) := metaRes.value / "unit" / "logback.xml",
      (generateLogback in Test) := true
    )

  lazy val stateCore = (u("state-core") / "state machine core" << main)

  lazy val stateReflect = u("state-reflect") / "state machine impl with scala.reflect" << stateCore

  // lazy val stateSm = (pb("state-sm") / "state machine impl with scala.meta" << stateCore)
  //   .antSrc
  //   .settingsV(
  //     prefixedName,
  //     addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full),
  //     scalacOptions += "-Xplugin-require:macroparadise",
  //     tryp.TekKeys.splain := false,
  //     logbackTemplate := metaRes.value / "unit" / "logback.xml",
  //     generateLogback := true
  //   )
  //   .logback("tag" -> "tryp")

  // lazy val spray = "spray" / "Spray/MongoDB model implementation" << app

  // lazy val lift = "lift" / "lift-ng/restrecord/scalajs model implementation" << app

  // lazy val unitSpray = tdp("unit-spray") ~ "unit/spray" / "Workaround for spray/specs2" << spray << unitMain

  // lazy val unitSlick = "unit-slick" ~ "unit/slick" / "Unit test helpers for slick" << slick << unitMain

  lazy val macrosUnit = tdp("macros-unit") ~ "unit/macros" / "Unit tests for macros" << unitMain << app

  // lazy val unit = tdp("unit").logback() / "Unit tests" << jvm << unitSpray << unitSlick

  lazy val debug = tdp("debug") / "Debugging" << main

  lazy val jvm = mpb("jvm").export / "Metaproject for non-android deps" << main

  lazy val public = mpb("public")
    .settingsV(publish := ())
    .aggregate(core.!, app.!, macros.!, main.!, test.!, unitMain.!, jvm.!, stateCore.!, stateReflect.!)

  lazy val all = mpb("all")
    .aggregate(core.!, app.!, test.!, macros.!, main.!)

  override lazy val macroConsole =
    metaProject("macro-console")
      .macroConsole
      .settingsV(publish := ())
      .map(_.disablePlugins(CoursierPlugin))
}
