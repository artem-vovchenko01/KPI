package quickcheck

import org.scalacheck.Prop
import org.scalacheck.Properties
import org.junit._

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Test.{check, Result, Failed, PropException}

object IndividualTaskSuite extends IndividualTask

class CheckSuite {
  def checkFunc(p: Properties): Unit = {
    def fail = throw new AssertionError(
      s"Function doesn`t satisfy all the properties")

    check(asProp(p))(identity) match {
      case r: Result => r.status match {
        case _: Failed => () // OK: scalacheck found a counter example!
        case p: PropException => p.e match {
          case e: NoSuchElementException => () // OK: the implementation throws NSEE
          case _ => fail
        }
        case _ => fail
      }
    }
  }

  /** Turns a Properties instance into a single Prop by combining all the properties */
  def asProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2).toSeq: _*)

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
