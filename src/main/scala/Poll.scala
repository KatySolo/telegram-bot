import java.util.Date

case class Poll(name: String, anonymous: Boolean, is_afterstop: Boolean,
                start: Option[Date], end: Option[Date],
                id: Int, owner: Long, questions: Vector[Question] = Vector.empty) {
  val ownerID: Long = 165755238

  def isStarted: Boolean = start.exists(new Date().after)
  def isStopped: Boolean = end.exists(new Date().after)
  def isActive: Boolean = isStarted && !isStopped

  override def toString: String = {
    s"""id: $id
       |name: $name
       |anonymous: $anonymous
       |afterstop: $is_afterstop
       |start date: ${start.getOrElse("undefined")}
       |end date: ${end.getOrElse("undefined")}
       |in process: $isActive
       |questions: ${questions.zipWithIndex.map(q => "\n\t" + viewQuestion(q._1, q._2)).mkString("\n")}""".stripMargin
  }

  private def viewQuestion(q: Question, id: Int): String = {
    val variants =
      if (q.variants.isEmpty) "Empty"
      else q.variants.zipWithIndex.map(o => s"\t${o._2}: ${o._1}").mkString("\n")
    s"""Question [$id]
       |   Name: ${q.text}
       |   Type: ${q.questionType}
       |   Options:
       |$variants
     """.stripMargin
  }
}
