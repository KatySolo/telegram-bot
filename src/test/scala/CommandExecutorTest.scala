import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Date

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import Executor._
import CommandParser._
import org.joda.time.DateTimeUtils

import scala.util.Try

class CommandExecutorTest extends FlatSpec with BeforeAndAfterEach {

  override def afterEach(): Unit = {
    Polls = Map.empty
  }

  "Command Executor" should
    "create a new poll" in {
    val start = "15:00:00 98:07:30"
    val end = "08:00:00 05:09:01"
    val dateFormat = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

    CommandParser.parse(CommandParser.command(165755238),
      "/create_poll (my_poll_t1) (no) (continuous) ("+start+") ("+ end+")").get()
    val poll = Polls(1)
    assert (poll.name == "my_poll_t1")
    assert (!poll.anonymous)
    assert (!poll.is_afterstop)
    assert (poll.start == Try(dateFormat.parse(start)).toOption)
    assert (poll.end == Try(dateFormat.parse(end)).toOption)

  }

  it should "give each poll unique ID" in {
    CommandParser.parse(CommandParser.command(165755238), "/create_poll (my_poll_t2)").get()
    CommandParser.parse(CommandParser.command(165755238), "/create_poll (my_poll_t2)").get()
    val result = Polls(2).id
    assert (result != Polls(3).id)
  }

  it should "have no effect after try to stop not started poll" in {
    CommandParser.parse(CommandParser.command(165755238), "/create_poll (my_poll_t3)").get()
    CommandParser.parse(CommandParser.command(165755238), "/stop_poll (4)").get()
    assert(!Polls(4).isActive)

  }

  it should "stop started poll" in {
    CommandParser.parse(CommandParser.command(165755238), "/create_poll (my_poll_t5)").get()
    CommandParser.parse(CommandParser.command(165755238), "/stop_poll (5)").get()
    assert(!Polls(5).isActive)
    CommandParser.parse(CommandParser.command(165755238), "/stop_poll (5)").get()
    assert(!Polls(5).isActive)
  }
  //
  it should "show all created polls" in {
    CommandParser.parse(CommandParser.command(1), "/create_poll (my_poll_t5)").get()
    CommandParser.parse(CommandParser.command(1), "/create_poll (my_poll_t5)").get()
    assert (Polls.size == 2)
  }
  //
  it should "show results of currently running poll" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
      "/begin (8)",
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
      "/end",
      "/start_poll (8)"))
    instructionExecutor(666, List("/begin (8)",
      "/answer (0) (0)"))
    val result = CommandParser.parse(CommandParser.command(666), "/view").get()

    assert (result == "poll results")
  }
  //
  it should "show results of finished polls" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
      "/begin (9)",
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
      "/end",
      "/start_poll (9)"))
    instructionExecutor(165755238, List("/stop_poll (9)"))
    CommandParser.parse(CommandParser.command(666), "/begin (9)").get()
    val poll = CommandParser.parse(CommandParser.command(666), "/view").get()
    assert (poll == "poll results")
  }
  it should "not be able to change poll after start" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
      "/begin (10)",
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
      "/end",
      "/start_poll (10)",
      "/begin (10)"))
    val result = CommandParser.parse(CommandParser.command(666),"/add_question (Ваше мнение о мероприятии?) (open)").get()
    assert(result == "Sorry, poll is running")
  }

  it should "add new question in poll" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
      "/begin (11)"))

    CommandParser.parse(CommandParser.command(165755238),
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе").get()

    assert (Polls(11).questions.size == 1)
  }

  it should "delete question from poll" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
      "/begin (12)",
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
      "/delete_question (1)"))
  }

  it should "answer question in the poll" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
      "/begin (13)",
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
      "/end",
      "/start_poll (13)"))
    instructionExecutor(666, List("/begin (13)",
      "/answer (0) (0)"))
  }

  //  it should "cannot answer if poll has not started" in {
  //    val start = LocalDateTime.now().plusDays(1).toString.formatted("hh:mm:ss yy:MM:dd");
  //    val end = LocalDateTime.now().plusDays(2).toString.formatted("hh:mm:ss yy:MM:dd");
  //
  //    instructionExecutor(165755238, List(
  //      "/create_poll (t3) (no) (continuous) ("+start+") ("+ end+")",
  //      "/ ()"
  //
  //    ))
  //    CommandParser.parse(CommandParser.command(165755238),
  //      "/create_poll (my_poll_t1) (no) (continuous) ("+start+") ("+ end+")")
  //    val poll = Polls(1)
  //
  //  }

  it should "not be able to start poll manually if start time has been written" in {
    val formatter = DateTimeFormatter.ofPattern("hh:mm:ss yy:MM:dd")
    val start = LocalDateTime.now().plusDays(1).format(formatter)
    val end = LocalDateTime.now().plusDays(2).format(formatter)

    CommandParser.parse(CommandParser.command(165755238),
      "/create_poll (t10) (no) (continuous) ("+start+") ("+ end+")").get()
    val result = CommandParser.parse(CommandParser.command(165755238),"/start_poll (14)").get()
    assert(result == "Fail on starting")
  }


  it should "not be able to finish poll manually if end time has been written" in {
    val formatter = DateTimeFormatter.ofPattern("hh:mm:ss yy:MM:dd")
    val start = LocalDateTime.now().minusDays(1).format(formatter)
    val end = LocalDateTime.now().plusDays(2).format(formatter)

    CommandParser.parse(CommandParser.command(165755238),
      "/create_poll (t10) (no) (continuous) ("+start+") ("+ end+")").get()
    println(Polls(15))

    val result = CommandParser.parse(CommandParser.command(165755238),"/stop_poll (15)").get()
    assert(result == "Fail on stopping")
  }


  it should "be able to take part only in active polls" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
      "/begin (16)",
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
      "/end"))

    CommandParser.parse(CommandParser.command(666), "/begin (16)").get()
    val result = CommandParser.parse(CommandParser.command(666), "/answer (16) (1)").get()
    assert(result == "Poll is not running")

  }
  it should "not be able to change poll if you are not a creator" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
      "/begin (17)",
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
      "/end"))

    val sas = CommandParser.parse(CommandParser.command(666), "/begin (17)").get()
    printf(sas)
    val result = CommandParser.parse(CommandParser.command(666),
      "/add_question (Куда идем завтра((пятница))?) (open)" ).get()
    assert(result == "You are not poll owner")
  }

  it should "switch to the poll" in {
    instructionExecutor(165755238, List("/create_poll (my_poll1)",
      "/begin (18)",
      "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
      "/end",
      "/start_poll (18)"))

    instructionExecutor(666, List("/begin (18)","/answer (0) (0)"))
    val result = CommandParser.parse(CommandParser.command(666), "/answer (0) (1)").get()
    assert (result == "Only one answer is allowed")
  }


  //  - участвовать в опросе может любой пользователь, но управлять им - только создавший
  //    - анонимность заключается в том, что не сохраняются ответы пользователей (только факт ответа и результат)
  ////  - видимость результатов "afterstop" - результаты опроса можно посмотреть только после его окончания, "continuous" - в процессе
  ////    - если время начала опроса задано, то бот автоматически стартует опрос в это время и вручную его стартовать нельзя
  ////  - если время конца опроса задано, то бот автоматически завершает опрос в это время и вручную его завершить нельзя
  ////  - участвовать можно только в активных опросах (опросы которые были начаты, но еще не завершены)
  //  - после старта опроса изменять его нельзя!

  def instructionExecutor(id: Int, in: List[String]): Unit = {
    in.map(CommandParser.parse(CommandParser.command(id), _).get())
  }
}