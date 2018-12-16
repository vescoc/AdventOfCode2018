import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

import java.io.{File, FileFilter}

lazy val testLibraryDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

lazy val commonSettings = Seq(
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-encoding", "UTF-8",
    "-unchecked",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  Test / parallelExecution := false,
  excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"
)

lazy val root = project.in(file("."))

lazy val aoc = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    commonSettings
  )
  .jsSettings(
    libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "0.9.6"),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv,
    scalaJSUseMainModuleInitializer := true,
    packageJSDependencies / skip := false,
    Compile / sourceGenerators += Def.task {
      val re = s"""input-(\\d+)\\.data""".r

      val baseDir = (root / baseDirectory).value / "aoc" / "resources"
      val sourceDir = (Compile / sourceManaged).value

      val sources = for {
        resourceFile <- baseDir listFiles(
          new FileFilter() {
            def accept(pathname: File): Boolean = {
              pathname.getName() match {
                case re(_) =>
                  true
                case _ =>
                  false
              }
            }
          }
        )
      } yield {
        val name = resourceFile getName() match {
          case re(n) =>
            s"Input$n"
        }

        val sourceFile = sourceDir / s"${name}.scala"

        if (!sourceFile.exists() || sourceFile.lastModified() < resourceFile.lastModified()) {
          val content = IO.read(resourceFile)

          val scalaCode = s"""package aoc

object $name extends Source.ContentProvider {
 final val content = raw\"\"\"$content\"\"\"
}
"""

          IO.write(sourceFile, scalaCode)
        }

        sourceFile
      }

      sources.toSeq
    }.taskValue
  )

lazy val aocJS = aoc
  .js
  .enablePlugins(ScalaJSPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.5" % Test
    )
  )

lazy val aocJVM = aoc
  .jvm
  .settings(
    Compile / unmanagedResourceDirectories += (root / baseDirectory).value / "aoc" / "resources",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )

excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"
