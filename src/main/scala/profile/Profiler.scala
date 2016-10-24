package profile

import collection.mutable.{Map,ListBuffer}
import org.slf4j.LoggerFactory

object Head {
  override def toString = {
    val name = "name"
    val ncalls = "ncalls"
    val tottime = "tottime"
    val average = "average"
    val mean = "50%"
    val upper = "95%"
    val max = "max"

    f"$name%20s $ncalls%10s $tottime%11s $average%9s $mean%9s $upper%9s $max%9s"
  }
}

case class Stat(
  name: String,
  runList: ListBuffer[Long]
) {
  val ncalls = runList.size
  val tottime = toSeconds(runList.sum)
  val average = tottime / ncalls

  val ordered = runList.sorted
  val mean  = toSeconds(ordered((ncalls * 0.50).toInt))
  val upper = toSeconds(ordered((ncalls * 0.95).toInt))
  val max   = toSeconds(ordered.last)

  private def toSeconds(nanotime: Long): Double =
    nanotime / 1000000000.0

  private def toMs(nanotime: Long): Double =
    nanotime / 1000000.0

  override def toString =
    f"$name%20s $ncalls%10d $tottime%11.5f $average%9.6f $mean%9.6f $upper%9.6f $max%9.6f"
}

object Profiler {
  val log = LoggerFactory.getLogger("Profiler")
  val runs = Map[String, ListBuffer[Long]]()

  private def add(name: String, timeDiff: Long): Unit = {
    if (! runs.contains(name))
      runs += (name -> ListBuffer[Long]())

    runs(name) += timeDiff
  }

  def profile[R](name: String, block: => R, variance: Boolean = true): R = {
    val begin = System.nanoTime()
    val result = block // call-by-name

    add(name, System.nanoTime() - begin)

    result
  }

  def profileMany[R](name: String, block: => R): Unit = {
    profileMany(name, 1000000, block)
  }

  def profileMany[R](name: String, limit: Int, block: => R): Unit = {
    val begin = System.nanoTime()
    var i = 0
    while (i < limit) {
      block // call-by-name
      i += 1
    }

    add(name, System.nanoTime() - begin)
  }

  private def getStats: List[Stat] = {
    runs.toList.map(run => Stat(run._1, run._2)).sortBy(_.tottime)
  }

  def printComplete: Unit = {
    log.info(Head.toString)

    getStats.map(stat => {
      log.info(stat.toString)
    })
  }

  def printShort: Unit = {
    val name = "name"
    val tottime = "tottime"

    log.info(f"$name%20s $tottime%11s")

    getStats.foreach(stat => {
      log.info(f"${stat.name}%20s ${tottime}%11s")
    })
  }

  def print(key: String): Unit = {
    val tottime = Stat(key, runs(key)).tottime
    log.info(f"$key took $tottime%1.2f seconds")
  }

  def clear: Unit = runs.clear
}
