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
    val minH1 = findMin(h1)
    val minH2 = findMin(h2)
    val minMelded = findMin(melded)
    if (minH1 < minH2) minMelded == minH1 else minMelded == minH2
  }

  property("heapIsOrderedWhileDeleting") = forAll { (h: H) =>
    if (isEmpty(h))
      true
    else
      findAllMin(h, findMin(h))
  }

  property("heapMeldIsOrderedWhileDeleting") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    if (isEmpty(melded))
      true
    else
      findAllMin(melded, findMin(melded))
  }

  property("heapMeldReturnsOrderedSequence") = {
    val h1 = insert(-1, insert(0, insert(1, empty)))
    val h2 = insert(-2, insert(10, empty))
    val h3 = meld(h1, h2)
    checkEach(h3, List(-2, -1, 0, 1, 10))
  }

  def checkEach(h: H, list: List[Int]): Boolean = {
    isEmpty(h) && list.isEmpty ||
      (!isEmpty(h) &&
        findMin(h) == list.head &&
        checkEach(deleteMin(h), list.tail))
  }

  //Returns the heap passed in if and only if the it is empty, or its minimum
  //is less than the minimum of each heap following the deletion of the minimum.
  def findAllMin(h: H, i: Int): Boolean = {
    val remainder = deleteMin(h)
    isEmpty(remainder) ||
      (ord.lteq(i, findMin(remainder)) && findAllMin(remainder, findMin(remainder)))

  }

  property("meldHeapsAndDelete") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    if (min1 < min2) findMin(deleteMin(h)) <= min2 else findMin(deleteMin(h)) <= min1
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
