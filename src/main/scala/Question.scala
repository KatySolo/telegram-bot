case class Question(text: String, questionType: Question.Value,
                    answers1: List[(Option[Long], String)] = List.empty,
                    answers2: List[(Option[Long], Int)] = List.empty,
                    answers3: List[(Option[Long], Set[Int])] = List.empty,
                    variants: List[String] = List.empty,
                    users: List[Long] = List.empty) {
  def answer(user: Long, answer: String, anon: Boolean): Question =
    copy(users = users :+ user, answers1 = answers1 :+ (if (anon) None else Some(user), answer))
  def answer(user: Long, answer: Int, anon: Boolean): Question =
    copy(users = users :+ user, answers2 = answers2 :+ (if (anon) None else Some(user), answer))
  def answer(user: Long, answer: Set[Int], anon: Boolean): Question =
    copy(users = users :+ user, answers3 = answers3 :+ (if (anon) None else Some(user), answer))

  override def toString: String = {
    if (questionType != Question.open)
      s"\n\t$text [$questionType]\n\tVariants:" +
        s"${variants.zipWithIndex.map(e =>
          s"\n\t\t${e._1}: ${stat(if (questionType == Question.choice) answers2 else answers3, e._2)}%").mkString("\n")}"
    else
      s"\n\t$text [$questionType]\n\tAnswers:${answers1.map(_._2).toSet[String].map(e =>
        s"\n\t\t$e: ${stat(answers1, e)}%").mkString("\n")}"
  }

  private def stat[T](list: List[(Option[Long], T)], ans: T) = (list.count(_._2 == ans) / users.length.toFloat * 100).toInt
}

object Question extends Enumeration {
  val open, choice, multi = Value

  def GetValue(text: String): Question.Value = {
    text match {
      case "choice" => Question.choice
      case "multi" => Question.multi
      case _ => Question.open
    }
  }


}
