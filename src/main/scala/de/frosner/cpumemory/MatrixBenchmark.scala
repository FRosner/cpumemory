package de.frosner.cpumemory

import scala.util.Random

object MatrixBenchmark extends App {

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

  val s = args(0).toInt
  val (a, b) = (Array.fill(s)(Array.fill(s)(Random.nextDouble())),
                Array.fill(s)(Array.fill(s)(Random.nextDouble())))

  args(1) match {
    case "1" => mult1(a, b, s)
    case "2" => mult2(a, b, s)
  }

}
