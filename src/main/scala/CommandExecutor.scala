import java.text.SimpleDateFormat
import java.util.Date

import scala.util.Try
import scala.util.parsing.combinator._

object CommandExecutor extends RegexParsers {
  private val id = Stream.from(1).iterator
  private val dateFormat = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  var Polls: Map[Int, Poll] = Map.empty
  val Questions: Map[Int, Question] = Map.empty


  def command = createPoll | simpleCommand | commandsWithoutArgs

  def createPoll: Parser[Poll] = "/create_poll" ~> anyWord ~ (anonymous | success(true)) ~
    (visibility | success(true)) ~ date ~ date ^^ {
    case name ~ anon ~ vis ~ start ~ stop => Poll(name, anon, vis, start, stop, id.next())
  }

  def simpleCommand: Parser[Unit] = "/" ~> ("delete_poll" | "start_poll" |
    "stop_poll" | "result") ~ idx ^^ { case commandType ~ idx => commandType match {
    case "delete_poll" => delete_poll(idx)
    case "start_poll" => start_poll(idx)
    case "result" => get_results(idx)
    case "stop_poll" => stop_poll(idx)
  }
  }

  def commandsWithoutArgs: Parser[Unit] = "/" ~> ("list" | "end" | "view") ^^ {
    case "list" => get_list()
//    case "end" => end()
//    case "view" => view()
  }

  def date: Parser[Option[Date]] = ("(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")" | "".r) ^^ {
    date => Try(dateFormat.parse(date)).toOption
  }

  def answers: Parser[Array[String]] = ("(" ~> anyWord <~ ")").* ^^ {_.toArray}

  def anyWord: Parser[String] = "(" ~> "[^)]*".r <~ ")" ^^ {_.toString}

  def idx: Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ {_.toInt}
  def anonymous: Parser[Boolean] = "(" ~> ("yes" | "no") <~ ")" ^^ {_.toString == "yes"}
  def visibility: Parser[Boolean] = "(" ~> ("afterstop" | "continuous") <~ ")" ^^ {_.toString == "afterstop"}


  def parse(action: String): Unit = {
    val commands = action.split(" ")
    commands(0) match {
      case "/create_poll" => create_poll(commands)
      case "/list" => get_list()
      case "/delete_poll" => delete_poll(commands(1).toInt)
      case "/start_poll" => start_poll(commands(1).toInt)
      case "/stop_poll" => stop_poll(commands(1).toInt)
      case "/result" => get_results(commands(1).toInt)
      case _ => println("Unknown command")
    }
  }

  def create_poll(str: Array[String]): Unit = {
    val name = str(1)
    val anon = str.lift(2).getOrElse("yes") == "yes"
    val AS = str.lift(3).getOrElse("afterstop") == "afterstop"
    val start : Option[Date] = for(x <- str.lift(4)) yield dateFormat.parse(x)
    val end : Option[Date] = for(x <- str.lift(5)) yield dateFormat.parse(x)

    val poll = Poll(name, anon, AS, start, end, id.next())
    Polls += poll.id -> poll

    println(s"Your poll id: ${poll.id}")
  }

  def get_list(): Unit = for (p <- Polls.values) println(s"Poll: ${p.name} with id: ${p.id}")

  def delete_poll(id: Int): Unit = {
    if (Polls.contains(id)) {
      Polls -= id
      println("Successfully deleted")
    }
    else println("Fail on deleting")
  }

  def start_poll(id: Int): Unit = {
    if (Polls.contains(id) && Polls(id).start.isEmpty) {
      Polls(id).isRunning = true
      println("Poll was started")
    }
    else println("Fail on starting")
  }

  def stop_poll(id: Int): Unit = {
    if (Polls.contains(id) && Polls(id).end.isEmpty) {
      Polls(id).isRunning = false
      println("Poll was stopped")
    }
    else println("Fail on stopping")
  }

  def get_results(id: Int): Unit = {
    if (Polls.contains(id) && Polls(id).isRuned){
      if (Polls(id).is_afterstop) println("I can't get result until ending")
      else Questions.mkString("\n")
    }
  }
}
