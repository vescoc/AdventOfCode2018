package aoc

trait SourceContract {
  trait ResourceSource {
    def getLines(): Iterator[String]
    def mkString: String
  }

  def fromResource(resource: String): ResourceSource
}
