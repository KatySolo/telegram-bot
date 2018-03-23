import java.util.Date

case class Poll(name: String, anonymous: Boolean, is_afterstop: Boolean, start: Option[Date], end: Option[Date], id: Int) {
  var isRunning: Boolean = isRun.getOrElse(false)

  private def isRun: Option[Boolean] = for (x <- start; y <- end; now = new Date())
    yield (now after x) && (now before y)

  def isRuned: Boolean = {
    if (isRun.getOrElse(false)) isRunning = true
    isRunning
  }
}
