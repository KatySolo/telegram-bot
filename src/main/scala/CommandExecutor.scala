import java.text.SimpleDateFormat
import java.util.Date

import scala.util.Try
import scala.util.parsing.combinator._

object command_executor extends RegexParsers {
  private val id = Stream.from(1).iterator
  private val dateFormat = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  var Polls: Map[Int, Poll] = Map.empty
  val Questions: Map[Int, Question] = Map.empty

  def command = createPoll | simpleCommand | complexCommand | addQuestion

  def createPoll: Parser[String] = "/create_poll" ~> anyWord ~ (anonymous | success(true)) ~
    (visibility | success(true)) ~ date ~ date ^^ {
      case name ~ anon ~ vis ~ start ~ stop =>
        Poll(name, anon, vis, start, stop, id.next())
        s"Your poll id: $id"
  }

  def simpleCommand: Parser[String] = "/" ~> ("delete_poll" | "start_poll" |
    "stop_poll" | "result" | "begin" | "delete_question") ~ idx ^^ {
      case command ~ idx => command match {
        case "delete_poll" => delete_poll(idx)
        case "start_poll" => start_poll(idx)
        case "result" => get_results(idx)
        case "stop_poll" => stop_poll(idx)
        case "begin" => begin(idx)
        case "delete_question" => delete_question(idx)
      }
  }

  def complexCommand: Parser[String] = "/" ~> ("list" | "end" | "view") ^^ {
    case "list" => get_list()
    case "end" => end()
    case "view" => view()
  }

  def addQuestion: Parser[String] = "/add_question" ~ anyWord ~ ("(" ~> ("open" | "choice" | "multi") <~ ")") ~
    answers ^^ { case _ ~ name ~ questionType ~ answers => add_question(name, Question.GetValue(questionType), answers) }

  def date: Parser[Option[Date]] = ("(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")" | "".r) ^^ {
    date => Try(dateFormat.parse(date)).toOption }

  def answers: Parser[Array[String]] = ("(" ~> anyWord <~ ")").* ^^ {_.toArray}
  def anyWord: Parser[String] = "(" ~> "[^)]*".r <~ ")" ^^ {_.toString}
  def idx: Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ {_.toInt}
  def anonymous: Parser[Boolean] = "(" ~> ("yes" | "no") <~ ")" ^^ {_.toString == "yes"}
  def visibility: Parser[Boolean] = "(" ~> ("afterstop" | "continuous") <~ ")" ^^ {_.toString == "afterstop"}


//  def parse(action: String): Unit = {
//    val commands = action.split(" ")
//    commands(0) match {
//      case "/create_poll" => create_poll(commands)
//      case "/list" => get_list()
//      case "/delete_poll" => delete_poll(commands(1).toInt)
//      case "/start_poll" => start_poll(commands(1).toInt)
//      case "/stop_poll" => stop_poll(commands(1).toInt)
//      case "/result" => get_results(commands(1).toInt)
//      case _ => println("Unknown command")
//    }
//  }

  def create_poll(str: Array[String]): String = {
    val name = str(1)
    val anon = str.lift(2).getOrElse("yes") == "yes"
    val AS = str.lift(3).getOrElse("afterstop") == "afterstop"
    val start : Option[Date] = for(x <- str.lift(4)) yield dateFormat.parse(x)
    val end : Option[Date] = for(x <- str.lift(5)) yield dateFormat.parse(x)

    val poll = Poll(name, anon, AS, start, end, id.next())
    Polls += poll.id -> poll

    s"Your poll id: ${poll.id}"
  }

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
      if (Polls(id).is_afterstop) "I can't get result until ending"
      Questions.mkString("\n")
    }
    ???
  }

  def add_question(str: String, value: Question.Value, strings: Array[String]) = {
    ???
  }

  def begin(i: Int) = {???}

  def delete_question(i: Int) = {???}

  def view() = {???}

  def end() = {???}

}
