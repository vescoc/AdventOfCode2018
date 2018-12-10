package aoc

import org.scalajs.dom
import dom.document
import dom.html

object Day10 {
  val SmallDelta = 1
  val BigDelta = 100

  val re = """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>""".r

  case class Point(x: Int, y: Int)
  case class Velocity(dx: Int, dy: Int)

  case class Star(p: Point, v: Velocity) {
    def time(t: Int): Star =
      copy(p = Point(p.x + v.dx * t, p.y + v.dy * t))
  }

  def main(args: Array[String]): Unit = {
    val input = Input10.content.lines.map {
      _ match {
        case re(x, y, vx, vy) =>
          Star(Point(x.toInt, y.toInt), Velocity(vx.toInt, vy.toInt))
      }
    }.toList

    val currentTimeParagraph = document.createElement("p").asInstanceOf[html.Paragraph]
    document.body.appendChild(currentTimeParagraph)

    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = (0.95 * dom.window.innerWidth).toInt
    canvas.height = (0.95 * dom.window.innerHeight).toInt
    document.body.appendChild(canvas)

    var currentTime = 0

    dom.window.addEventListener(
      "keydown",
      (e: dom.KeyboardEvent) => {
        e.keyCode match {
          case 39 => // right arrow
            currentTime += SmallDelta
            render()
          case 37 => // left arrow
            currentTime -= SmallDelta
            render()
          case 38 => // up arrow
            currentTime += BigDelta
            render()
          case 40 => // down arrow
            currentTime -= BigDelta
            render()
          case _ =>
          // nothing
        }
      }
    )

    def render(): Unit = {
      println(s"render $currentTime")

      ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)

      val currentPoints = input
        .map { _.time(currentTime) }

      val (minPoint, maxPoint) = currentPoints
        .foldLeft((Point(Int.MaxValue, Int.MaxValue), Point(Int.MinValue, Int.MinValue))) { (acc, star) =>
          (
            Point(Math.min(acc._1.x, star.p.x), Math.min(acc._1.y, star.p.y)),
            Point(Math.max(acc._2.x, star.p.x), Math.max(acc._2.y, star.p.y))
          )
        }

      currentTimeParagraph.innerHTML =
        s"Current time: $currentTime -- [$minPoint - $maxPoint] [${maxPoint.x - minPoint.x}, ${maxPoint.y - minPoint.y}]"

      currentPoints
        .foreach { star =>
          ctx.fillRect(star.p.x.toDouble, star.p.y.toDouble, 1, 1)
        }
    }

    render()
  }
}
