package scalashop

import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object IndividualTask extends App {
    def partFunc = new PartialFunction[(Int, Int), BigInt] {
      def apply(params: (Int, Int)) = if (params._1 > params._2) fact(params._1, 1) else params._2
      def isDefinedAt(params: (Int, Int)): Boolean = {
        val x = params._1
        val k = params._2
        x > 0 && (x > k || (x > 1 && x < k) )
      }
    }

    def func(x: Int, k: Int): Option[BigInt] = {
      partFunc.lift(x, k)
    }

    def fact(x: BigInt, res: BigInt): BigInt = {
      if (x > 0) fact(x - 1, res * x)
      else res
    }

    val k = 20
    val future = Future {
      (-250 to 250).par.map(x => func(x, k))
    }

  println(Await.result(future, 30.seconds))
}
