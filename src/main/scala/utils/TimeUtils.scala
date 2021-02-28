package utils

import org.joda.time.Duration

object TimeUtils {
  def elapsedTime(startTimestamp: Long): Duration = {
    new Duration(System.currentTimeMillis() - startTimestamp)
  }

  def printElapsedTime(startTimestamp: Long): Unit = {
    val period = elapsedTime(startTimestamp).toPeriod()
    println(
      f"${period.getHours}%02d:${period.getMinutes}%02d:" +
        f"${period.getSeconds}%02d:${period.getMillis}"
    )
  }
}
