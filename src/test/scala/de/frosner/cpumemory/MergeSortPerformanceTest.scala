package de.frosner.cpumemory

import java.util

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import scala.util.{Random, Sorting}

object MergeSortPerformanceTest extends Bench.OfflineReport {

  /*
    - lazy vs strict map implementation on sequences
    - sorted on list, array, vector + arrays.sort
    - concatenation vs builder
   */

  val size = Gen.enumeration("size")(List.iterate(1, 7)(_ * 10): _*)
  val list = for { s <- size } yield List.fill(s)(Random.nextInt)

  /*
  sort :: Ord a => [a] -> [a]

sort []    =  []
sort [x]  =  [x]
sort xs   =  merge (sort left) (sort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] xs = xs
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y      = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
   */

  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (Nil, l) => l
      case (l, Nil) => l
      case (xh :: xt, yh :: yt) =>
        if (xh <= yh)
          xh :: merge(xt, ys)
        else
          yh :: merge(xs, yt)
    }

  def sort(xs: List[Int]): List[Int] =
    xs match {
      case Nil         => Nil
      case head :: Nil => head :: Nil
      case xs =>
        val (left, right) = xs.splitAt(xs.length / 2)
        merge(sort(left), sort(right))
    }

  performance of "List" in {
    measure method "sorted" in {
      using(list) in { l =>
        l.sorted
      }
    }
    measure method "merge-sort" in {
      using(list) in { l =>
        Sorting.stableSort(l)
      }
    }
  }

}
