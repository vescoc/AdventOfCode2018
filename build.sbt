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
  excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"
)

lazy val scalajs = (project in file("scalajs"))
  .enablePlugins(ScalaJSPlugin)
  .settings (
    name := "AdventOfCode2018-scalajs",
    libraryDependencies ++= testLibraryDependencies ++
      Seq("org.scala-js" %%% "scalajs-dom" % "0.9.6"),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv,
    scalaJSUseMainModuleInitializer := true,
    packageJSDependencies / skip := false,
    Compile / sourceGenerators += Def.task {
      val re = s"""input-(\\d+)\\.data""".r

      val baseDir = baseDirectory.value / "src/main/resources"
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

          val scalaCode = s"""|package aoc
          |
          |object $name {
          |  final val content = raw\"\"\"$content\"\"\"
          |}
          |""".stripMargin

          IO.write(sourceFile, scalaCode)
        }

        sourceFile
      }

      sources.toSeq
    }.taskValue,
    commonSettings
  )

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2018",
    libraryDependencies ++= testLibraryDependencies,
    commonSettings
  )

excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"

