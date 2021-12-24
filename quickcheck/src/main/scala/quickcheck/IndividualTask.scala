package quickcheck
import org.scalacheck._
import Prop._
import scala.math.{min, max}

abstract class IndividualTask extends Properties("Individual Task") {
    def fact(a: Int, cumul: BigInt): BigInt = {
        if (a == 1 || a == 0) cumul
        else fact(a - 1, cumul * a)
    }

    def myFuncPart: PartialFunction[(Int, Int), BigInt] = {
        case (x, k) 
            if x > 1 && x < k => BigInt(k)
        case (x, k)
            if x > k && x >= 0 => fact(x, 1)
    }

    def lifting(x: Int, k: Int): Option[BigInt] = {
        myFuncPart.lift(x, k)
    }

    val prop1Gen = for {
        x <- Gen.choose(1, 100)
        k <- Gen.choose(x, 5000)
    } yield ( x, k )

    property("prop1") = forAll(prop1Gen) { ( t: Tuple2[Int, Int] ) =>
        val x = t._1
        val k = t._2
        (x > 1) ==> {
            if (lifting(x, k).compare(Some(BigInt(k))) == 0 || lifting(x, k).compare(None) == 0) true
            else false
        }
    }

    val prop2Gen = for {
        k <- Gen.choose(-5000, 5000)
        x <- Gen.choose(max(0, k), 10000)
    } yield (x, k)
    property("prop2") = forAll(prop2Gen) { ( t: Tuple2[Int, Int] ) =>
        val x = t._1
        val k = t._2
        if (lifting(x, k).compare(Some(fact(x, 1))) == 0) true
        else false
    }
}
