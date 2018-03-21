package de.frosner.cpumemory

import java.util

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import scala.collection.SeqView
import scala.util.Random

object SortingPerformanceTest extends Bench.OfflineReport {

  /*
    - lazy vs strict map implementation on sequences
    - sorted on list, array, vector + arrays.sort
    - concatenation vs builder
   */

  val size = Gen.enumeration("size")(List.iterate(1, 7)(_ * 10): _*)
  val list = for { s <- size } yield List.fill(s)(Random.nextInt)
  val array = for { l <- list } yield l.toArray
  val vector = for { l <- list } yield l.toVector

  performance of "sorted" in {
    measure method "List.sorted" in {
      using(list) in { l =>
        l.sorted
      }
    }
    measure method "Array.sorted" in {
      using(array) in { l =>
        l.sorted
      }
    }
    measure method "Vector.sorted" in {
      using(vector) in { l =>
        l.sorted
      }
    }
  }

  performance of "Arrays.sort" in {
    measure method "in-place" in {
      using(array) in { l =>
        util.Arrays.sort(l)
      }
    }
    measure method "in-place-defensive-copy" in {
      using(array) in { l =>
        val newArray = new Array[Int](l.length)
        Array.copy(l, 0, newArray, 0, l.length)
        util.Arrays.sort(newArray)
      }
    }
  }

}
