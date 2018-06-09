import java.security.PolicySpi
import java.util.Date

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object Executor {
  private val id = Stream.from(1).iterator
  private var context: Map[Long, Int] = Map.empty

  var Polls: Map[Int, Poll] = Map.empty

  def create_poll(name: String, anon: Boolean, vis: Boolean, start: Option[Date], stop: Option[Date], owner: Long): String = {
    val idx = id.next()
    Polls += idx -> Poll(name, anon, vis, start, stop, idx, owner)
    s"Your poll id: $idx"
  }

  def delete_poll(id: Int, user: Long): String =
    Polls.get(id).map(p => {
      if (user != p.owner) "Permission denied!"
      Polls -= id
      "Successfully deleted"
    }).getOrElse(s"Poll with id: $id doesn't exist")

  def start_poll(id: Int, user: Long): String =
    Polls.get(id).map(p => {
      if (user != p.owner) "Permission denied!"
      if (p.start.isDefined) "Start time is already defined"
      Polls += id -> p.copy(start = Option(new Date()))
      "Poll was started"
    }).getOrElse(s"Poll with id: $id doesn't exist")

  def stop_poll(id: Int, user: Long): String =
    Polls.get(id).map(p => {
      if (user != p.owner) "Permission denied!"
      if (p.isStopped) "Poll had already finished"
      if (p.end.isDefined) "Stop time is already defined"
      Polls += id -> p.copy(end = Option(new Date()))
      "Poll was stopped"
    }).getOrElse(s"Poll with id: $id doesn't exist")

  def get_results(id: Int): String =
    Polls.get(id).map(p => {
      if (p.isActive && p.is_afterstop) "You can't get result until ending"
      s"""Poll ${p.name}:
         |${p.questions.map(_.toString).mkString("\n")}""".stripMargin
    }).getOrElse(s"Poll with id: $id doesn't exist")

  def get_list(): String = Polls.values.mkString("\n")

  def begin(id: Int, user: Long): String =
    Polls.get(id).map(p => {
      if (user != p.owner) "Permission denied!"
      if (p.isStarted) "You can't modify the poll after start"
      Polls += id -> p.copy(end = Option(new Date()))
      context += user -> id
      s"You were switched to poll $id context"
    }).getOrElse(s"Poll with id: $id doesn't exist")

  def end(user: Long): String =
    context.get(user).map(id => {
      context -= user
      s"Work with the poll $id is finished"
    }).getOrElse("Context mode disabled")

  def add_question(text: String, _type: Question.Value, variants: List[String], user: Long): String =
    context.get(user).map(id =>
      Polls.get(id).map(p => {
        if (user != p.owner) "Permission denied!"
        if (p.isStarted) "You can't modify the poll after start"
        _type match {
          case Question.open =>
            Polls += id -> p.copy(questions = p.questions :+ Question(text, _type))
          case _ =>
            if (variants.isEmpty || variants.length < 2) "It must be at least 2 variants"
            Polls += id -> p.copy(questions = p.questions :+ Question(text, _type, variants = variants))
        }
        s"Sucсess!\r\nQuestion added to Poll with id: $id"
      }).getOrElse(s"Poll with id: $id doesn't exist")
    ).getOrElse("Context mode disabled")

  def delete_question(i: Int, user: Long): String =
    context.get(user).map(id =>
      Polls.get(id).map(p => {
        if (user != p.owner) "Permission denied!"
        if (p.isStarted) "You can't modify the poll after start"
        Polls += id -> p.copy(questions = p.questions.patch(i, Nil, 1))
        s"Sucсess!"
      }).getOrElse(s"Poll with id: $id doesn't exist")
    ).getOrElse("Context mode disabled")

  def answer(i: Int, answer: String, user: Long): String = {
    context.get(user).map(id =>
      Polls.get(id).map(p => {
        if (p.questions.lengthCompare(i) <= 0) s"Question with id: $i doesn't exist"
        if (p.isStopped) "Poll's time is up"
        if (!p.isStarted) "Poll hasn't started yet"
        if (p.questions(i).users.contains(user)) "You already answered this question"
        answerr(user, answer, p.anonymous, p.questions(i), (newQuestion: Question) =>
          Polls += id -> p.copy(questions = p.questions.patch(i, Seq(newQuestion), 1)))
        s"Sucсess!"
      }).getOrElse(s"Poll with id: $id doesn't exist")
    ).getOrElse("Context mode disabled")
  }

  private def answerr(user: Long, answer: String, anon: Boolean, question: Question, f: Question => Unit): String = {
    question.questionType match {
      case Question.open =>
        f(question.answer(user, answer, anon))
        "Answer submitted"

      case Question.choice =>
        Try(answer.toInt - 1).toOption match {
          case Some(ans) =>
            if (!question.variants.isDefinedAt(ans)) "Variant's index out of range"
            f(question.answer(user, ans, anon))
            "Answer submitted"
          case None => "Incorrect answer format"
        }

      case Question.multi =>
        Try(answer.split(" ").map(_.toInt - 1)).toOption match {
          case Some(ans) =>
            if (ans.distinct.lengthCompare(ans.length) != 0) "Variant must be distinct"
            if (!ans.forall(question.variants.isDefinedAt)) "Variant's index out of range"
            f(question.answer(user, ans.toSet, anon))
            "Answer submitted"
          case None => "Incorrect answer format"
        }
    }
  }

  def view(user: Long): String =
    context.get(user).map(id =>
      Polls.get(id).map(_.toString).getOrElse(s"Poll with id: $id doesn't exist")
    ).getOrElse("Context mode disabled")
}
