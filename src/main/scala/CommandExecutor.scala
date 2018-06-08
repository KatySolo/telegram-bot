import java.text.SimpleDateFormat
import java.util.Date

import scala.util.Try
import scala.util.parsing.combinator._


object CommandExecutor extends RegexParsers {
  private val id = Stream.from(1).iterator
  private val dateFormat = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  var Polls: Map[Int, Poll] = Map.empty

  def command(userID: Long) = createPoll(userID) | simpleCommand(userID) | complexCommand | addQuestion(userID) | answer(userID)

  def createPoll(userID: Long): Parser[String] = "/create_poll" ~> anyWord ~ (anonymous | success(true)) ~
    (visibility | success(true)) ~ date ~ date ^^ {
      case name ~ anon ~ vis ~ start ~ stop =>
        val poll = Poll(name, anon, vis, start, stop, id.next())
        Polls += poll.id -> poll
        s"Your poll id: ${poll.id}"
  }

  def simpleCommand(userID: Long): Parser[String] = "/" ~> ("delete_poll" | "start_poll" |
    "stop_poll" | "result" | "begin" | "delete_question") ~ idx ^^ {
      case command ~ idx => command match {
        case "delete_poll" => delete_poll(idx)
        case "start_poll" => start_poll(idx)
        case "result" => get_results(idx)
        case "stop_poll" => stop_poll(idx)
        case "begin" => begin(idx)
        case "delete_question" => delete_question(idx, userID)
      }
  }

  def complexCommand: Parser[String] = "/" ~> ("list" | "end" | "view") ^^ {
    case "list" => get_list()
    case "end" => end()
    case "view" => view()
  }

  def answer(userID: Long): Parser[String] = "/answer" ~ idx ~ anyWord ^^ { case _ ~ i ~ ans => answer(i, ans) }

  def addQuestion(userID: Long): Parser[String] = "/add_question" ~> question ~ ("(" ~> ("open" | "choice" | "multi") <~ ")").? ~
    answers ^^ { case name ~ qType ~ answers => add_question(name, Question.GetValue(qType.getOrElse("open")), answers, userID) }

  def date: Parser[Option[Date]] = ("(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")" | "".r) ^^ {
    date => Try(dateFormat.parse(date)).toOption }

  def answers: Parser[Array[String]] = ".+".r.* ^^ {_.map(_.trim()).toArray}
  def question: Parser[String] = "\\(((\\(\\()|(\\)\\))|[^()])*\\)".r ^^ {q => q.substring(1, q.length - 1).replace("((", "(").replace("))", ")")}
  def anyWord: Parser[String] = "(" ~> "[^)]*".r <~ ")"
  def idx: Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ {_.toInt}
  def anonymous: Parser[Boolean] = "(" ~> ("yes" | "no") <~ ")" ^^ {_ == "yes"}
  def visibility: Parser[Boolean] = "(" ~> ("afterstop" | "continuous") <~ ")" ^^ {_ == "afterstop"}

//  def parse(action: String): Parser[String] = {
//    val commands = action.split(" ")
//    commands(0) match {
//      case "/create_poll" => create_poll(commands)
//      case "/list" => get_list()
//      case "/delete_poll" => delete_poll(commands(1).toInt)
//      case "/start_poll" => start_poll(commands(1).toInt)
//      case "/stop_poll" => stop_poll(commands(1).toInt)
//      case "/result" => get_results(commands(1).toInt)
//      case _ => "Unknown command"
//    }
//  }

//  def create_poll(str: Array[String]): String = {
//    val name = str(1)
//    val anon = str.lift(2).getOrElse("yes") == "yes"
//    val AS = str.lift(3).getOrElse("afterstop") == "afterstop"
//    val start : Option[Date] = for(x <- str.lift(4)) yield dateFormat.parse(x)
//    val end : Option[Date] = for(x <- str.lift(5)) yield dateFormat.parse(x)
//
//    val poll = Poll(name, anon, AS, start, end, id.next())
//    Polls += poll.id -> poll
//
//    s"Your poll id: ${poll.id}"
//  }

  def get_list(): String = Polls.values.mkString("\n")

  def delete_poll(id: Int): String = {
    if (!Polls.contains(id))
      "Fail on deleting"
    Polls -= id
    "Successfully deleted"
  }

  def start_poll(id: Int): String = {
    if (!(Polls.contains(id) && Polls(id).start.isEmpty))
      "Fail on starting"
    Polls(id).isRunning = true
    "Poll was started"
  }

  def stop_poll(id: Int): String = {
    if (!(Polls.contains(id) && Polls(id).end.isEmpty))
      "Fail on stopping"
    Polls(id).isRunning = false
    "Poll was stopped"
  }

  def get_results(id: Int): String = {
    if (Polls.contains(id) && Polls(id).isRuned){
      if (Polls(id).is_afterstop) "You can't get result until ending"
//      Questions.mkString("\n") TODO
    }
    ???
  }

  private var contextId: Int = -1

  def begin(i: Int): String = {
    if (!Polls.contains(i)) "This poll doesn't exist"
    contextId = i
    s"You were switched to poll $i context"
  }

  def end(): String = {
    if (contextId < 0) "context mode disabled"
    val i = contextId
    contextId = -1
    s"Work with the poll $i is finished"
  }

  def add_question(text: String, value: Question.Value, answers: Array[String], userID: Long): String = {
    Polls.get(contextId) match {
      case Some(p) =>
        if (p.ownerID != userID) "You are not poll owner"
        if (p.isRunning) "Sorry, poll is running"
        val qId = /*id.next()*/ p.id
        p.add_question(Question(text, value, answers.map(x => x -> (0, List.empty)).toMap, answers.toVector), qId)
        s"Sucсess!\r\nPoll id: $contextId\r\nQuestion id: $qId"
      case None => "Context mode disabled"
    }
  }

  def delete_question(i: Int, userID: Long): String = {
    Polls.get(contextId) match {
      case Some(p) =>
        if (p.ownerID != userID) "You are not poll owner"
        if (p.isRunning) "Sorry, poll is running"
        p.delete_question(i)
        s"Sucсess!"
      case None => "Context mode disabled"
    }
  }

  def answer(i: Int, answer: String): String = {
//    Polls.get(contextId) match {
//      case Some(p) =>
////        if (!p.isRunning) "Poll is not running"
////        p.Questions.get(i) match { case Some(q) => q.answer(answer, userID) }
//      case None => "Context mode disabled"
//    }
    ???
  }

  def view(): String = {
    if (contextId < 0) "context mode disabled"
    Polls.get(contextId).map(_.toString).getOrElse("Current poll doesn't exist")
  }
}
