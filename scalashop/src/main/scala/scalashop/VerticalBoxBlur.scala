package scalashop

import org.scalameter._
import scalashop.HorizontalBoxBlur.blur

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
      val pixels = for {
        col <- from until end
        row <- 0 until src.height
      } yield (row, col)
      pixels.foreach(pixel => dst.update(pixel._2, pixel._1, boxBlurKernel(src, pixel._2, pixel._1, radius)))
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
      if (numTasks != 0) {
        val step = if (numTasks >= src.width) 1 else src.width / numTasks
        val initSplits = 0 to src.width by step
        val splits = initSplits.updated(initSplits.size - 1, src.width)
        splits.zip(splits.tail).map {
          case (from, end) => task {
              blur(src, dst, from, end, radius)
          }
        }.foreach(task => task.join())
      }
  }

}
