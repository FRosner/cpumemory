package de.frosner.cpumemory

import java.util

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

object CacheSizeTest extends Bench.OfflineReport {

  /*
  hw.l1icachesize: 32768
  hw.l1dcachesize: 32768
  hw.l2cachesize: 262144
  hw.l3cachesize: 4194304
   */

  val intPerKb = 1024 / 4
  val intPerMb = intPerKb * 1024

  val sizes = Gen.range("size")(0, 8, 1)

  val arrayss = for {
    size <- sizes.map(_ * intPerKb)
  } yield (0 until size).reverse.toArray

  val lists = rangeTo(_.toList)
  val arrays = rangeTo(_.toArray)
  val vectors = rangeTo(_.toVector)

  def rangeTo[X](f: Range => X): Gen[X] =
    for {
      size <- sizes.map(_ * intPerMb)
    } yield f((0 until size).reverse)

//  performance of "Array" in {
//    measure method "sort" in {
//      using(arrays) in { a =>
//        util.Arrays.sort(a)
//      }
//    }
//  }

//  performance of "sort" in {
//    measure method "List" in {
//      using(lists) in { l =>
//        l.sorted
//      }
//    }
//    measure method "Array(sorted)" in {
//      using(arrays) in { l =>
//        l.sorted
//      }
//    }
//    measure method "Array(sort)" in {
//      using(arrays) in { l =>
//        util.Arrays.sort(l)
//      }
//    }
//    measure method "Vector" in {
//      using(vectors) in { l =>
//        l.sorted
//      }
//    }
//  }

  performance of "Vector" in {
    def f(i: Int): Int = i + 1
    def g(i: Int): Int = i - 10
    measure method "map" in {
      using(vectors) in { l =>
        l.map(i => g(f(i)))
      }
    }
    measure method "sorted" in {
      using(vectors) in { l =>
        l.sorted
      }
    }
//    measure method "mapmapmap" in {
//      using(vectors) in { l =>
//        l.map(f).map(g)
//      }
//    }
  }

  performance of "List" in {
    def f(i: Int): Int = i + 1
    def g(i: Int): Int = i - 10
    measure method "map" in {
      using(lists) in { l =>
        l.map(i => g(f(i)))
      }
    }
    measure method "sorted" in {
      using(lists) in { l =>
        l.sorted
      }
    }
//    measure method "mapmapmap" in {
//      using(lists) in { l =>
//        l.map(f).map(g)
//      }
//    }
  }

}
