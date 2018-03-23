case class Question(text: String, questionType: Question.Value, answers: Array[String])

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
