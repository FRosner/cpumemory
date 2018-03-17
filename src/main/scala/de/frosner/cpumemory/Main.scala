package de.frosner.cpumemory

import java.lang.management.ManagementFactory
import java.util

import java.lang.instrument.Instrumentation

import scala.util.Random

object Main extends App {

  val os = ManagementFactory.getOperatingSystemMXBean
  val rt = ManagementFactory.getThreadMXBean

  def withTime(f: () => Any): Long = {
    val before = rt.getCurrentThreadCpuTime
    f()
    (rt.getCurrentThreadCpuTime - before) / 1000
  }

  def measure(runs: Int, kbs: Seq[Int]): Seq[(Int, (Long, Long))] = {
    def run(kb: Int): (Long, Long) = {
      val size = kb * 1024 / 8
      val data = Seq.fill(size)(Random.nextLong()).toArray
      val indices = data.indices.toArray
      val randomIndices =
        Seq.fill(size)(Random.nextDouble()).zipWithIndex.sortBy(_._1).map(_._2)

      def median(indices: Seq[Int]): Long = {
        val results = for {
          i <- 1 to runs
        } yield
          withTime { () =>
            for {
              i <- indices
            } {
              data(i)
            }
          }
        val sortedResults = results.sorted
        sortedResults(runs / 2)
      }

      (median(indices), median(randomIndices))
    }

    for {
      kb <- kbs
    } yield (kb, run(kb))
  }

  println("   kb" + " |" + f"     seq" + " |" + "     rdm")
  measure(1000, Stream.iterate(1)(_ * 2).take(11)).foreach {
    case (kb, (sequencialMs, randomMs)) =>
      println(
        f"$kb% 5d" + " |" + f"$sequencialMs% 8d" + " |" + f"$randomMs% 8d")
  }

}
