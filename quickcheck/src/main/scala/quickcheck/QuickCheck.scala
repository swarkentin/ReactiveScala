package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("size1") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    if (a < b)
      findMin(h) == a
    else
      findMin(h) == b
  }

  property("emptyOnDelete") = forAll { a: Int =>
    val before = insert(a, empty)
    val after = deleteMin(before)
    isEmpty(after)
  }

  property("minOfMeldIsOneOrOther") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    (findMin(melded) == findMin(h1)) ||
      (findMin(melded) == findMin(h2))
  }

  property("heapIsOrderedWhileDeleting") = forAll { (h: H) =>
    findMin(h) == findMin(findAllMin(h))
  }

  //Returns the heap passed in if and only if the it is empty, or its mimimum
  //is less than the minimum of each heap following the deletion of the minimum.
  def findAllMin(h: H): H = {
    if (!isEmpty(h)) {
      val min = findMin(h)
      val remainder = deleteMin(h)
      if (isEmpty(remainder) || ord.lteq(min, findMin(remainder)))
        h
      else
        ???
    }
    h
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(value(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
