package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.math.{min, max}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for (x <- arbitrary[Int]; h <- oneOf(const(empty), genHeap)) yield insert(x, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    findMin(insert(a, empty)) == a
  }

  property("minFromTwo") = forAll { (a: Int, b: Int) => 
     val h = insert( a, insert(b, empty) )
     findMin(h) == min(a, b)
  }

  property("deleteMinWithOneElem") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("minOfTwoHeaps") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1)) findMin( meld(h1, h2) ) == findMin(h2)
    else if (isEmpty(h2)) findMin( meld(h1, h2) ) == findMin(h1) 
    else {
      findMin( meld(h1, h2) ) == min( findMin(h1), findMin(h2) )
    }
  }

  property("sortAfterDeleteMins") = forAll { h1: H =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val newHeap = deleteMin(h)
        isEmpty(newHeap) || findMin(h) <= findMin(newHeap) && isSorted(newHeap)
      }
    }
    isSorted(h1)
  }

  property("getEmptyHeapAfterDeleteTwoElem") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    isEmpty(deleteMin(deleteMin(h)))
  }

  property("addElemBiggerThanMin") = forAll { (h: H, a: Int) => {
    if (isEmpty(h)) true
    else {
      if (a < findMin(h)) findMin(insert(a, h)) == a
      else findMin(insert(a, h)) == findMin(h)
    }
  }
  }

  property("unionTwoHeaps") = forAll { (h1: H, h2: H) =>
    def compTwoHeaps(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else findMin(h1) == findMin(h2) && compTwoHeaps(deleteMin(h1), deleteMin(h2))
    }
    if (isEmpty(h1)) true
    else {
      val min1 = findMin(h1)
      compTwoHeaps(meld(h1,h2), meld(deleteMin(h1), insert(min1, h2)))
    }
  }

}
