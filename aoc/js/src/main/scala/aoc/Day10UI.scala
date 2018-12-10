package aoc

import scala.scalajs.js.timers._
import org.scalajs.dom
import org.scalajs.dom.{document, ext, html}
import ext.KeyCode

import aoc.Day10._

object Day10UI {
  def main(args: Array[String]): Unit = {
    val currentTimeParagraph = document.createElement("p").asInstanceOf[html.Paragraph]
    document.body.appendChild(currentTimeParagraph)

    val canvas = document.createElement("canvas").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = (0.95 * dom.window.innerWidth).toInt
    canvas.height = (0.95 * dom.window.innerHeight).toInt
    document.body.appendChild(canvas)

    var currentTime = 0
    var autotuneMode = false
    var autotuneDirection = 0

    var timeoutHandle: SetTimeoutHandle = null
    var minArea: Long = Long.MaxValue
    var currentArea: Long = Long.MaxValue

    def startAutotune() =
      if (!autotuneMode) {
        autotuneMode = true
        autotuneDirection = SmallDelta

        currentTime += autotuneDirection

        render()
      }

    def stopAutotune() =
      if (autotuneMode) {
        clearTimeout(timeoutHandle)

        autotuneMode = false
        timeoutHandle = null

        render()
      }

    def changeAutotuneDirection() =
      if (autotuneDirection == 0 || autotuneDirection < 0)
        +1
      else
        -1

    def sameAutotuneDirection() =
      if (autotuneDirection == 0)
        +1
      else
        autotuneDirection

    def disableAutotuneMode() = {
      if (autotuneMode) {
        clearTimeout(timeoutHandle)
      }

      autotuneMode = false
      timeoutHandle = null
    }

    def addCurrentTime(delta: Int) = {
      disableAutotuneMode()
      currentTime += delta
      render()
    }

    dom.window.addEventListener(
      "keydown",
      (e: dom.KeyboardEvent) => {
        e.keyCode match {
          case KeyCode.Right =>
            addCurrentTime(SmallDelta)
          case KeyCode.Left =>
            addCurrentTime(-SmallDelta)
          case KeyCode.Up =>
            addCurrentTime(BigDelta)
          case KeyCode.Down =>
            addCurrentTime(-BigDelta)
          case KeyCode.A =>
            startAutotune()
          case KeyCode.P =>
            stopAutotune()
          case _ =>
          // nothing
        }
      }
    )

    def render(): Unit = {
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

      val area = (maxPoint.x - minPoint.x).toLong * (maxPoint.y - minPoint.y).toLong

      autotuneDirection =
        if (area == minArea)
          0
        else if (area > currentArea)
          changeAutotuneDirection()
        else
          sameAutotuneDirection()

      minArea = Math.min(minArea, area)
      currentArea = area

      currentTimeParagraph.innerHTML =
        s"""Current time: $currentTime -- [$minPoint - $maxPoint] [${maxPoint.x - minPoint.x}, ${maxPoint.y - minPoint.y}] Autotune: ${if (autotuneMode)
          "On"
        else "Off"}"""

      currentPoints
        .foreach { star =>
          ctx.fillRect(star.p.x.toDouble, star.p.y.toDouble, 1, 1)
        }

      if (autotuneMode)
        if (autotuneDirection == 0)
          stopAutotune()
        else
          timeoutHandle = setTimeout(1) {
            currentTime += autotuneDirection

            render()
          }
    }

    render()
  }
}
