package aoc

import scala.scalajs.js.timers._

import org.scalajs.dom
import org.scalajs.dom.{document, ext, html}

import aoc.Day17._

object Day17UI {
  val Mag = 6.0
  val Timeout = 1.0

  def main(args: Array[String]): Unit = {
    val ground = parse(Source.fromResource("input-17.data").getLines())

    val canvasWidth = (ground.max._1 - ground.min._1 + 10) * Mag
    val canvasHeight = (ground.max._2 - ground.min._2 + 10) * Mag

    val waterCount = document.createElement("p").asInstanceOf[html.Paragraph]
    val restWaterCount = document.createElement("p").asInstanceOf[html.Paragraph]
    val antsCount = document.createElement("p").asInstanceOf[html.Paragraph]

    val container = document.createElement("div").asInstanceOf[html.Div]
    container.style = s"""width:${Math.min(canvasWidth, dom.window.innerWidth).toInt}px;height:${Math
      .min(canvasHeight, dom.window.innerHeight)
      .toInt}px;overflow:auto;border:1px solid;"""

    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.style = "display:block;"
    canvas.width = canvasWidth.toInt
    canvas.height = canvasHeight.toInt

    container.appendChild(canvas)

    document.body.appendChild(waterCount)
    document.body.appendChild(restWaterCount)
    document.body.appendChild(antsCount)
    document.body.appendChild(container)

    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.translate(-(ground.min._1 - 5) * Mag, 0)
    ctx.scale(Mag, Mag)

    val ant = ground.ant()

    var lastVisited = Set.empty[(Int, Int)]
    var lastRest = Set.empty[(Int, Int)]
    var lastAnts = Set.empty[(Int, Int)]

    renderBackground()

    render()

    def render(): Unit = {
      ant.walk()

      val (visited, rest, ants) =
        ground.Ant.ants.foldLeft((Set.empty[(Int, Int)], Set.empty[(Int, Int)], Set.empty[(Int, Int)])) { (acc, p) =>
          (acc._1 ++ p._2.visitedPositions, acc._2 ++ p._2.restWater, acc._3 + p._2.currentPosition)
        }

      ctx.fillStyle = ext.Color.Blue.toHex
      (visited -- lastVisited).foreach { p =>
        val x = p._1.toDouble
        val y = p._2.toDouble
        ctx.fillRect(x, y, 1, 1)
      }
      lastVisited = visited

      ctx.fillStyle = ext.Color(0, 128, 255).toHex
      (rest -- lastRest).foreach { p =>
        val x = p._1.toDouble
        val y = p._2.toDouble
        ctx.fillRect(x, y, 1, 1)
      }
      lastRest = rest

      ctx.fillStyle = ext.Color.Red.toHex
      (ants -- lastAnts -- visited -- rest).foreach { p =>
        val x = p._1.toDouble
        val y = p._2.toDouble
        ctx.fillRect(x, y, 1, 1)
      }
      lastAnts = ants

      waterCount.innerHTML = s"Water: ${ant.water} (${visited.size})"
      restWaterCount.innerHTML = s"Rest Water: ${ant.restWater} (${rest.size})"
      antsCount.innerHTML = s"Ants: ${ants.size}"

      if (!ant.done)
        setTimeout(Timeout) {
          render()
        }

      ()
    }

    def renderBackground(): Unit = {
      ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)

      for {
        y <- (ground.min._2 - 1) to (ground.max._2 + 1)
        x <- (ground.min._1 - 1) to (ground.max._1 + 1)
      } {
        if (ground(x, y) == GroundElement.Clay) {
          ctx.fillStyle = ext.Color(128, 0, 64).toHex
          ctx.fillRect(x.toDouble, y.toDouble, 1, 1)
        }
      }
    }
  }
}
