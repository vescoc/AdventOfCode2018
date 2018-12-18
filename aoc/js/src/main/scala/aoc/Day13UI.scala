package aoc

import scala.scalajs.js.timers._

import org.scalajs.dom
import org.scalajs.dom.{document, ext, html}

import aoc.Day13._

object Day13UI {
  val timeout = 5.0
  val Mag = 0.95

  def main(args: Array[String]): Unit = {
    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]

    document.body.appendChild(canvas)

    canvas.width = (Mag * dom.window.innerWidth).toInt
    canvas.height = (Mag * dom.window.innerHeight).toInt

    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val problem = Problem(Source.fromResource("input-13.data").getLines())

    var carts = problem.carts

    ctx.setTransform(
      canvas.width * Mag / problem.TrackWidth,
      0.0,
      0.0,
      canvas.height * Mag / problem.TrackHeight,
      0.0,
      0.0
    )

    setTimeout(timeout) {
      tick()
    }

    def tick(): Unit = {
      carts = problem.tick(carts)._1

      render()

      setTimeout(timeout) {
        tick()
      }

      ()
    }

    def render(): Unit =
      //ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
      carts foreach { cart =>
        ctx.fillStyle = ext.Color.all(1 + cart.id % (ext.Color.all.size - 1)).toHex
        ctx.fillRect(cart.location.x.toDouble, cart.location.y.toDouble, 1, 1)
      }
  }
}
