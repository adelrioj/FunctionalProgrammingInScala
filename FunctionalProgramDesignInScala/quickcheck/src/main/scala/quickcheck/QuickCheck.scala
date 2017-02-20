package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

    lazy val genHeap: Gen[H] = for {
        n <- arbitrary[A]
        h <- frequency((1, Gen.const(empty)), (9, genHeap))
    } yield insert(n, h)

    implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

    property("gen1") = forAll { (h: H) =>
        val m = if (isEmpty(h)) 0 else findMin(h)
        findMin(insert(m, h)) == m
    }

    /*
    Coursera hints
     */
    property("If you insert any two elements into an empty heap, " +
      "finding the minimum of the resulting heap should get the smallest of the two elements back") =
      forAll { (n1: A, n2: A) =>
          val h = insert(n1, insert(n2, empty))
          findMin(h) == min(n1, n2)
      }

    property("If you insert an element into an empty heap, then delete the minimum, " +
      "the resulting heap should be empty") =
      forAll { (n: A) =>
          val h = insert(n, empty)
          isEmpty(deleteMin(h))
      }

    property("Given any heap, " +
      "you should get a sorted sequence of elements when continually finding and deleting min") =
      forAll { (h: H) =>
          def isSorted(heap: H): Boolean =
              if (isEmpty(heap))
                  true
              else {
                  val deletedHeap = deleteMin(heap)
                  isEmpty(deletedHeap) || (findMin(heap) <= findMin(deletedHeap) && isSorted(deletedHeap))
              }
          isSorted(h)
      }

    property("Finding a minimum of the melding of any two heaps " +
      "should return a minimum of one or the other") =
      forAll { (h1: H, h2: H) =>
          findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
      }

    /*
    Custom properties, trying to find bogus.
     */
    property("meld two arbitrary heaps, then remove min from 1 and insert into 2, meld the results. Compare.") =
      forAll { (h1: H, h2: H) =>
          def remMin(ts: H): List[Int] = {
              if (isEmpty(ts)) Nil
              else findMin(ts) :: remMin(deleteMin(ts))
          }
          val meld2 = meld(deleteMin(h1), insert(findMin(h1), h2))
          remMin(meld(h1, h2)) == remMin(meld2)
      }
}
