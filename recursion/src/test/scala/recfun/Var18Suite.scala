package recfun
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Var18Suite extends AnyFunSuite {
  import Main.var18
  test("var18: k=3,x=2") {
    assert(var18(3, 2) === 3)
  }

  test("var18: k=3,x=5") {
    assert(var18(3,5) === 120)
  }

//  test("var18: k=-4,x=-3") {
//    assert(var18(-4,-3) === "values are invalid")
//  }
//
//  test("var18: k=0, x=0") {
//    assert(var18(0,0) === "values are invalid")
//  }
}
