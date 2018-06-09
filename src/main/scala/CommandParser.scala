import scala.util.Try
import java.text.SimpleDateFormat
import java.util.Date

import Executor._

import scala.util.parsing.combinator.RegexParsers

object CommandParser extends RegexParsers {
  private val dateFormat = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  def command(userID: Long):Parser[() => String] = createPoll(userID) | simpleCommand(userID) | complexCommand(userID) | addQuestion(userID) | answer(userID)

  private def createPoll(userID: Long): Parser[() => String] = "/create_poll" ~> anyWord ~ (anonymous | success(true)) ~
    (visibility | success(true)) ~ date ~ date ^^ {
    case name ~ anon ~ vis ~ start ~ stop => () => create_poll(name, anon, vis, start, stop, userID)
  }

  private def simpleCommand(userID: Long): Parser[() => String] = "/" ~> ("delete_poll" | "start_poll" |
    "stop_poll" | "result" | "begin" | "delete_question") ~ idx ^^ {
    case command ~ idx => command match {
      case "delete_poll" => () => delete_poll(idx, userID)
      case "start_poll" => () => start_poll(idx, userID)
      case "result" => () => get_results(idx)
      case "stop_poll" => () => stop_poll(idx, userID)
      case "begin" => () => begin(idx, userID)
      case "delete_question" => () => delete_question(idx, userID)
    }
  }

  private def complexCommand(userID: Long): Parser[() => String] = "/" ~> ("list" | "end" | "view") ^^ {
    case "list" => () => get_list()
    case "end" => () => end(userID)
    case "view" => () => view(userID)
  }

  private def answer(userID: Long): Parser[() => String] = "/answer" ~> idx ~ anyWord ^^ {
    case i ~ ans => () => Executor.answer(i, ans, userID) }

  private def addQuestion(userID: Long): Parser[() => String] = "/add_question" ~> question ~
    ("("~>("open"|"choice"|"multi")<~")").? ~ variants ^^ {
      case name ~ qType ~ variants => () => add_question(name, Question.GetValue(qType.getOrElse("open")), variants, userID)
  }

  private def date: Parser[Option[Date]] = ("(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")" | "".r) ^^ {
    date => Try(dateFormat.parse(date)).toOption }

  private def question: Parser[String] = "\\(((\\(\\()|(\\)\\))|[^()])*\\)".r ^^ {
    q => q.substring(1, q.length - 1).replace("((", "(").replace("))", ")")}

  private def variants: Parser[List[String]] = ".+".r.* ^^ {_.map(_.trim())}

  private def anyWord: Parser[String] = "(" ~> "[^)]*".r <~ ")"

  private def idx: Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ {_.toInt}

  private def anonymous: Parser[Boolean] = "(" ~> ("yes" | "no") <~ ")" ^^ {_ == "yes"}

  private def visibility: Parser[Boolean] = "(" ~> ("afterstop" | "continuous") <~ ")" ^^ {_ == "afterstop"}
}
