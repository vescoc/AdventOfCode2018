package aoc

import org.scalajs.dom
import org.scalajs.dom.{document, ext, html}

import aoc.Day03._

object Day03UI {
  final val Mag = 0.95

  def main(args: Array[String]): Unit = {
    val (minX, minY, maxX, maxY) = input
      .foldLeft((Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue)) { (acc, r) =>
        (Math.min(acc._1, r.x), Math.min(acc._2, r.y), Math.max(acc._3, r.x + r.w), Math.max(acc._4, r.y + r.h))
      }

    val container = document.createElement("div").asInstanceOf[html.Div]
    container.style =
      s"width: ${Math.min((Mag * dom.window.innerWidth).toInt, maxX)}px;height:${Math.min((Mag * dom.window.innerHeight).toInt, maxY)}px;overflow:auto;border:1px solid;"

    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.style = "display:block;"

    container.appendChild(canvas)
    document.body.appendChild(container)

    canvas.width = maxX
    canvas.height = maxY

    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    implicit class RectangleColor(r: Rectangle) {
      def color(): ext.Color =
        ext.Color.all(1 + r.id % (ext.Color.all.size - 1))
    }

    def render(): Unit =
      input foreach { r =>
        ctx.fillStyle = r.color.toHex
        ctx.fillRect(r.x.toDouble, r.y.toDouble, (r.x + r.w).toDouble, (r.y + r.h).toDouble)
      }

    render()
  }
}
