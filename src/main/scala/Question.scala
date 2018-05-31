case class Question(text: String, questionType: Question.Value, answers: Map[String, (Int, List[Long])],
                    variants: Vector[String] = Vector.empty, users: List[Long] = List.empty){
  def answer(str: String, userId: Long): (Question, String) = {
    questionType match {
      case Question.open =>
        if (!answers.contains(str))
          (this.copy(answers = answers + (str -> (1, List(userId))), users = userId :: users), "Thank you for answer")
        val oldAnswer = answers.get(str)
//        answers += (str -> (oldAnswer.get._1 + 1, userId :: oldAnswer.get._2))
        (this.copy(answers = answers + (str -> (oldAnswer.get._1 + 1, userId :: oldAnswer.get._2)),
          users = userId :: users), "Thank you for answer")

      case Question.choice =>
        try {
          val ansVar = variants(str.toInt)
          val oldAnswer = answers.get(ansVar)
          (this.copy(answers = answers - ansVar + (ansVar -> (oldAnswer.get._1 + 1, userId :: oldAnswer.get._2)),
            users = userId :: users), "Thank you for choice")
        }
        catch { case _: Throwable => (this, "Wrong command") }

      case Question.multi =>
        try {
          val vars = str.split("\\s+").map(_.toInt).toList
          if (vars.toSet.size != vars.length) (this, "Don't repeat answers")
          (this.copy(answers = getUpdatedAnswers(vars, userId, answers), users = userId :: users), "thx")
        }
        catch { case _: Throwable => (this, "Wrong command") }
    }
  }

  def getUpdatedAnswers(numbers: List[Int], userId: Long, curAnswers: Map[String, (Int, List[Long])]): Map[String, (Int, List[Long])] = {
    if (numbers.isEmpty) curAnswers
    val ansVar = variants(numbers.head)
    val oldAnswer = curAnswers.get(ansVar)
    if (oldAnswer.get._2.contains(userId)) getUpdatedAnswers(numbers.drop(1), userId, curAnswers)
    else getUpdatedAnswers(numbers.drop(1), userId,
      curAnswers - ansVar + (ansVar -> (oldAnswer.get._1 + 1, userId :: oldAnswer.get._2)))
  }
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
