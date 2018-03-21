package de.frosner.cpumemory

import java.util

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import scala.collection.mutable._
import scala.util.Random

object BuilderPerformanceTest extends Bench.OfflineReport {

  /*
    - lazy vs strict map implementation on sequences
    - sorted on list, array, vector + arrays.sort
    - concatenation vs builder
   */

  val size = Gen.single("size")(10000)
  val numbers = for { s <- size } yield 0 to s
  val strings = for { range <- numbers } yield range.map(_.toString)
  val array = for {
    range <- numbers
  } yield range.toArray

  performance of "List" in {
    measure method "concatenation" in {
      using(numbers) in { xs =>
        xs.foldLeft(List.empty[Int])((l, x) => x :: l)
      }
    }
    measure method "builder +=" in {
      using(numbers) in { xs =>
        var b = List.newBuilder[Int]
        xs.foreach(x => b += x)
        b.result
      }
    }
    measure method "builder ++=" in {
      using(numbers) in { xs =>
        var b = List.newBuilder[Int]
        b ++= xs
        b.result
      }
    }
  }

  performance of "String" in {
    measure method "concatenation" in {
      using(strings) in { xs =>
        xs.foldLeft("")((l, x) => l + x)
      }
    }
    measure method "builder +=" in {
      using(strings) in { xs =>
        var b = StringBuilder.newBuilder
        xs.foreach(x => b + x)
        b.result
      }
    }
    measure method "builder ++=" in {
      using(strings) in { xs =>
        var b = StringBuilder.newBuilder
        b ++= xs.mkString
        b.result
      }
    }
  }

  performance of "Set" in {
    measure method "concatenation" in {
      using(numbers) in { xs =>
        xs.foldLeft(Set.empty[Int])((l, x) => l + x)
      }
    }
    measure method "builder +=" in {
      using(numbers) in { xs =>
        var b = Set.newBuilder[Int]
        xs.foreach(x => b += x)
        b.result
      }
    }
    measure method "builder ++=" in {
      using(numbers) in { xs =>
        var b = Set.newBuilder[Int]
        b ++= xs
        b.result
      }
    }
  }

  performance of "Array" in {
    measure method "copy" in {
      using(array) in { xs =>
        val newArray = new Array[Int](xs.length)
        Array.copy(xs, 0, newArray, 0, xs.length)
      }
    }
    measure method "loop =" in {
      using(array) in { xs =>
        var a = new Array[Int](xs.length)
        var i = 0
        while (i < xs.length) {
          a(i) = xs(i)
          i += 1
        }
      }
    }
    measure method "builder +=" in {
      using(array) in { xs =>
        var b = Array.newBuilder[Int]
        xs.foreach(x => b += x)
        b.result
      }
    }
    measure method "builder ++=" in {
      using(array) in { xs =>
        var b = Array.newBuilder[Int]
        b ++= xs
        b.result
      }
    }
  }

}
