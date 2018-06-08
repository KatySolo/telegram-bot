import java.util.Date

case class Poll(name: String, anonymous: Boolean, is_afterstop: Boolean, start: Option[Date], end: Option[Date], id: Int) {
  var isRunning: Boolean = isRun.getOrElse(false)
  var Questions: Map[Int, Question] = Map.empty

  val ownerID: Long = 165755238

  private def isRun: Option[Boolean] = for (x <- start; y <- end; now = new Date())
    yield (now after x) && (now before y)

  def isRuned: Boolean = {
    if (isRun.getOrElse(false)) isRunning = true
    isRunning
  }

  def add_question(question: Question, id: Int): Unit = Questions += id -> question

  def delete_question(id: Int): Unit = Questions -= id

  override def toString: String = {
    s"""id: $id
       |name: $name
       |anonymous: $anonymous
       |afterstop: $is_afterstop
       |start date: ${start.getOrElse("undefined")}
       |end date: ${end.getOrElse("undefined")}
       |in process: $isRunning"""
//       |current questions: ${questions.map(pair => "\n\tID: " + pair._1 + ". " + questionToString(pair._2)).mkString}\n""".stripMargin
  }
}
