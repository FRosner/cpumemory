package de.frosner.cpumemory

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import scala.collection.SeqView

object LazyStrictPerformanceTest extends Bench.OfflineReport {

  /*
    - lazy vs strict map implementation on sequences
    - sorted on list, array, vector + arrays.sort
    - concatenation vs builder
   */

  val size = Gen.single("size")(1000000)
  val strictList = for {
    s <- size
  } yield List.iterate(0, s)(_ + 1)
  val lazyList = for {
    l <- strictList
  } yield l.view.asInstanceOf[SeqView[Int, Seq[_]]]
  val iterator = for {
    l <- strictList
  } yield l.toIterator
  val stream = for {
    l <- strictList
  } yield l.toStream
  val strictListList = for {
    l <- strictList
  } yield l.toList

  val f: Int => Int = _ + 1
  val g: Int => Int = _ / 10
  val h: Int => Int = _ - 5
  val fs = List.fill(10)(f)
  val fsAndThen = fs.reduce(_ andThen _)

  performance of "strictList" in {
    measure method "map" in {
      using(strictList) in { l =>
        l.map(fsAndThen)
      }
    }
    measure method "mapmap" in {
      using(strictList) in { l =>
        fs.foldLeft(l)((l, f) => l.map(f))
      }
    }
  }

  performance of "lazyList" in {
    measure method "map" in {
      using(lazyList) in { l =>
        l.map(fsAndThen).force
      }
    }
    measure method "mapmap" in {
      using(lazyList) in { l =>
        fs.foldLeft(l)((l, f) => l.map(f)).force
      }
    }
  }

}
