package aoc

import scala.scalajs.js.timers._

import org.scalajs.dom
import org.scalajs.dom.{document, ext, html}

import aoc.Day18._
import aoc.Day18.LandscapeType._

object Day18UI {
  val Mag = 1.0
  val Timeout = 25.0

  def main(args: Array[String]): Unit = {
    val landscape = Landscape(Source.fromResource("input-18.data").getLines().toList)

    val canvasWidth = landscape.Width * Mag
    val canvasHeight = landscape.Height * Mag

    val container = document.createElement("div").asInstanceOf[html.Div]
    container.style =
      s"width:${Math.min(canvasWidth, dom.window.innerWidth)}px;height:${Math.min(canvasHeight, dom.window.innerHeight)}px;overflow:auto;border:1px solid;"

    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.style = "display:block;"

    container.appendChild(canvas)
    document.body.appendChild(container)

    canvas.width = canvasWidth.toInt
    canvas.height = canvasHeight.toInt

    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.setTransform(
      Mag,
      0.0,
      0.0,
      Mag,
      0.0,
      0.0
    )

    def render(landscape: Landscape): Unit = {
      ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)

      for {
        x <- 0 until landscape.Width
        y <- 0 until landscape.Height
      } {
        ctx.fillStyle = {
          landscape(x, y) match {
            case OpenGround => ext.Color.Yellow
            case Trees      => ext.Color.Green
            case Lumberyard => ext.Color(128, 0, 0)
          }
        }.toHex

        ctx.fillRect(x.toDouble, y.toDouble, 1, 1)
      }

      setTimeout(Timeout) {
        render(landscape.evolve())
      }

      ()
    }

    render(landscape)
  }
}
