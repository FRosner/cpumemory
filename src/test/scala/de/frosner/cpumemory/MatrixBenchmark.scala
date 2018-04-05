package de.frosner.cpumemory

import org.scalameter.api._

import scala.util.Random
import org.scalameter.picklers.Implicits._

object MatrixBenchmark extends Bench.OfflineReport {
  val size = Gen.enumeration("size")(10, 50, 100, 500, 1000, 3000)
  val ms = for { s <- size } yield
    (Array.fill(s)(Array.fill(s)(Random.nextDouble())),
     Array.fill(s)(Array.fill(s)(Random.nextDouble())))

  def mult1(m1: Array[Array[Double]],
            m2: Array[Array[Double]],
            size: Int): Array[Array[Double]] = {
    var res = Array.fill(size)(new Array[Double](size))
    var i = 0
    while (i < size) {
      var j = 0
      while (j < size) {
        var k = 0
        while (k < size) {
          res(i)(j) += m1(i)(k) * m2(k)(j)
          k += 1
        }
        j += 1
      }
      i += 1
    }
    res
  }

  def mult2(m1: Array[Array[Double]],
            m2: Array[Array[Double]],
            size: Int): Array[Array[Double]] = {
    var m2t = Array.fill(size)(new Array[Double](size))
    var x = 0
    while (x < size) {
      var y = 0
      while (y < size) {
        m2t(x)(y) = m2(y)(x)
        y += 1
      }
      x += 1
    }

    var res = Array.fill(size)(new Array[Double](size))
    var i = 0
    while (i < size) {
      var j = 0
      while (j < size) {
        var k = 0
        while (k < size) {
          res(i)(j) += m1(i)(k) * m2t(j)(k)
          k += 1
        }
        j += 1
      }
      i += 1
    }
    res
  }

  def mult3(m1: Array[Array[Double]],
            m2: Array[Array[Double]],
            size: Int): Array[Array[Double]] = {
    var m2t = Array.fill(size)(new Array[Double](size))
    for (x <- 0 until size) {
      for (y <- 0 until size) {
        m2t(x)(y) = m2(y)(x)
      }
    }

    var res = Array.fill(size)(new Array[Double](size))
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        for (k <- 0 until size) {
          res(i)(j) += m1(i)(k) * m2t(j)(k)
        }
      }
    }
    res
  }

//  val m1e = Array(Array(1d, 2d), Array(3d, 4d))
//  val m2e = Array(Array(5d, 6d), Array(7d, 8d))
//  println(mult1(m1e, m2e, 2).map(_.mkString(" ")).mkString("\n"))
//  println(mult2(m1e, m2e, 2).map(_.mkString(" ")).mkString("\n"))
//  println(mult3(m1e, m2e, 2).map(_.mkString(" ")).mkString("\n"))

  val s = 1000
  val (a, b) = (Array.fill(s)(Array.fill(s)(Random.nextDouble())),
                Array.fill(s)(Array.fill(s)(Random.nextDouble())))
  mult2(a, b, s)

//  performance of "Matrix Multiplication" in {
//    measure method "mult1" in {
//      using(ms) in {
//        case (m1, m2) =>
//          mult1(m1, m2, m1.length)
//      }
//    }
//    measure method "mult2" in {
//      using(ms) in {
//        case (m1, m2) =>
//          mult2(m1, m2, m1.length)
//      }
//    }
//    measure method "mult3" in {
//      using(ms) in {
//        case (m1, m2) =>
//          mult3(m1, m2, m1.length)
//      }
//    }
//  }
}
