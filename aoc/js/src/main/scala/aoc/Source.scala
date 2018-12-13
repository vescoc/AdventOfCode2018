package aoc

import scala.scalajs.reflect.Reflect
import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation

object Source extends SourceContract {
  val patternRe = """input-(\d+)\.data""".r

  @EnableReflectiveInstantiation
  trait ContentProvider {
    def content: String
  }

  def fromResource(resource: String) = new ResourceSource {
    val content = resource match {
      case patternRe(digits) =>
        Reflect
          .lookupLoadableModuleClass(s"aoc.Input${digits}$$")
          .get
          .loadModule()
          .asInstanceOf[ContentProvider]
          .content
    }

    def getLines() = content.lines

    def mkString = content
  }
}
