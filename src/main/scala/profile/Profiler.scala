package profile

import collection.mutable.Map

object Profiler {
  val stats = Map[String, (Int, Long)]()

  def profile[R](name: String, block: => R): R = {
    val tBegin = System.nanoTime()
    val result = block // call-by-name
    val tEnd = System.nanoTime()

    if (! stats.contains(name))
      stats += (name -> (0, 0L))

    val tmp = stats(name)
    stats += (name -> (tmp._1 + 1, tmp._2 + tEnd - tBegin))

    result
  }

  def profileMany[R](name: String, block: => R): Unit = {
    profileMany(name, 1000000, block)
  }

  def profileMany[R](name: String, limit: Int, block: => R): Unit = {
    val tBegin = System.nanoTime()
    var i = 0
    while (i < limit) {
      block // call-by-name
      i += 1
    }
    val tEnd = System.nanoTime()

    if (! stats.contains(name))
      stats += (name -> (0, 0L))

    val tmp = stats(name)
    stats += (name -> (tmp._1 + 1, tmp._2 + tEnd - tBegin))
  }

  def printComplete: Unit = {
    val name = "name"
    val ncalls = "ncalls"
    val time = "tottime"
    val percall = "percall"
    println(f"$name%30s $ncalls%10s $time%11s $percall%9s")

    stats.toSeq.sortBy(_._2._2).map {
      case (name: String, (ncalls: Int, time: Long)) => {
        val seconds = time / 1000000000.0
        val percall = seconds / ncalls
        println(f"$name%30s $ncalls%10d $seconds%11.5f $percall%9.5f")
      }
    }
  }

  def printShort: Unit = {
    val name = "name"
    val time = "time"
    println(f"$name%30s $time%11s")

    stats.toSeq.sortBy(_._2._2).map {
      case (name: String, (ncalls: Int, time: Long)) => {
        val seconds = time / 1000000000.0
        println(f"$name%30s $seconds%11.5f")
      }
    }

    println
  }

  def clear: Unit = stats.clear
}
