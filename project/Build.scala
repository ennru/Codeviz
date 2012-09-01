import sbt._
import sbt.Keys._
import com.typesafe.startscript.StartScriptPlugin
import com.typesafe.sbtscalariform.ScalariformPlugin
import com.typesafe.sbtscalariform.ScalariformPlugin.ScalariformKeys

object CodevizBuild extends Build {
  val Organization = "codeviz"
  val Version      = "1.0-SNAPSHOT"
  val ScalaVersion = "2.9.2"

  lazy val codeviz = Project(
    id = "codeviz",
    base = file("."),
    settings = defaultSettings ++
      Seq(StartScriptPlugin.stage in Compile := Unit),
      aggregate = Nil // Seq(common, processor, service, client)
  )

  lazy val defaultSettings = Defaults.defaultSettings ++ formatSettings ++ Seq(
    resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",

    // compile options
    scalacOptions ++= Seq("-encoding", "UTF-8", "-optimise", "-deprecation", "-unchecked"),
    javacOptions  ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),

    // disable parallel tests
    parallelExecution in Test := false
  )

  lazy val formatSettings = ScalariformPlugin.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test    := formattingPreferences
  )

  def formattingPreferences = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
      .setPreference(RewriteArrowSymbols, true)
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
  }
}

object Dependencies {
  import Dependency._
  val codeviz = Seq(scalaTest, jUnit)
}

object Dependency {
  object Version {
    val Scalatest = "1.6.1"
    val JUnit     = "4.5"
  }

  // ---- Application dependencies ----


  // ---- Test dependencies ----

  val scalaTest   = "org.scalatest"       % "scalatest_2.9.0"          % Version.Scalatest  % "test"
  val jUnit       = "junit"               % "junit"                    % Version.JUnit      % "test"
}
