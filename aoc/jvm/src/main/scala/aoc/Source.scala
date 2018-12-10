package aoc

import scala.io.{Source => S}

object Source extends SourceContract {
  def fromResource(resource: String) = new ResourceSource {
    def getLines() = S.fromResource(resource).getLines()

    def mkString = S.fromResource(resource).mkString
  }
}
