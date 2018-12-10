package aoc

object Source extends SourceContract {
  def fromResource(resource: String) = new ResourceSource {
    val content = resource match {
      case "input-01.data" => Input01.content
      case "input-02.data" => Input02.content
      case "input-03.data" => Input03.content
      case "input-04.data" => Input04.content
      case "input-05.data" => Input05.content
      case "input-06.data" => Input06.content
      case "input-07.data" => Input07.content
      case "input-08.data" => Input08.content
      case "input-09.data" => Input09.content
      case "input-10.data" => Input10.content
    }

    def getLines() = content.lines

    def mkString = content
  }
}
