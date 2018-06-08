import java.text.SimpleDateFormat

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import CommandExecutor._

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

    CommandExecutor.parse(CommandExecutor.command(165755238),
      "/create_poll (my_poll_t1) (no) (continuous) ("+start+") ("+ end+")")
    val poll = Polls(1)
    assert (poll.name == "my_poll_t1")
    assert (!poll.anonymous)
    assert (!poll.is_afterstop)
    assert (poll.start == Try(dateFormat.parse(start)).toOption)
    assert (poll.end == Try(dateFormat.parse(end)).toOption)

  }

  it should "give each poll unique ID" in {
    CommandExecutor.parse(CommandExecutor.command(165755238), "/create_poll (my_poll_t2)")
    CommandExecutor.parse(CommandExecutor.command(165755238), "/create_poll (my_poll_t2)")
    val result = Polls(2).id
    assert (result != Polls(3).id)
  }

  it should "have no effect after try to stop not started poll" in {
    CommandExecutor.parse(CommandExecutor.command(165755238), "/create_poll (my_poll_t3)")
    CommandExecutor.parse(CommandExecutor.command(165755238), "/stop_poll (4)")
    assert(!Polls(4).isRunning)

  }

  it should "stop started poll" in {
    CommandExecutor.parse(CommandExecutor.command(165755238), "/create_poll (my_poll_t5)")
    CommandExecutor.parse(CommandExecutor.command(165755238), "/stop_poll (5)")
    assert(!Polls(5).isRunning)
    CommandExecutor.parse(CommandExecutor.command(165755238), "/stop_poll (5)")
    assert(!Polls(5).isRunning)
  }
//
  it should "show all created polls" in {
    CommandExecutor.parse(CommandExecutor.command(1), "/create_poll (my_poll_t5)")
    CommandExecutor.parse(CommandExecutor.command(1), "/create_poll (my_poll_t5)")
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
    val result = CommandExecutor.parse(CommandExecutor.command(666), "/view").get
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
    val result = CommandExecutor.parse(CommandExecutor.command(666), "/view").get
    assert (result == "poll results")
  }
  it should "not be able to change poll after start" in {
    instructionExecutor(165755238, List("/create_poll (my_poll)",
                                "/begin (10)",
                                "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе",
                                "/end",
                                "/start_poll (10)",
                                "/begin (10)"))
    val result = CommandExecutor.parse(CommandExecutor.command(666),"/add_question (Ваше мнение о мероприятии?) (open)").get
    assert(result == "строка с ошибкой тут")
  }

  it should "add new question in poll" in {
    instructionExecutor(1, List("/create_poll (my_poll)",
                                "/begin (11)",
                                "/add_question (Куда идем завтра((пятница))?) (choice)\n В бар!\n В кино!\n Сидим в офисе"))
    val pollQuestions = Polls(11).Questions(11)
//    assert (pollQuestions)
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

  it should "start in time if start time has been written" in {

  }

  it should "not be able to start poll manualy if start time has been written" in {

  }
  it should "finish in time if end time has been written" in {

  }

  it should "not be able to finish poll manualy if end time has been written" in {

  }
  it should "be able to take part only in active polls" in {

   }
  it should "not ne able to change poll if you are not a creator" in {

  }

  it should "switch to the poll" in {

  }

  it should "exit chosen poll" in{

  }


//  - участвовать в опросе может любой пользователь, но управлять им - только создавший
//    - анонимность заключается в том, что не сохраняются ответы пользователей (только факт ответа и результат)
////  - видимость результатов "afterstop" - результаты опроса можно посмотреть только после его окончания, "continuous" - в процессе
////    - если время начала опроса задано, то бот автоматически стартует опрос в это время и вручную его стартовать нельзя
////  - если время конца опроса задано, то бот автоматически завершает опрос в это время и вручную его завершить нельзя
////  - участвовать можно только в активных опросах (опросы которые были начаты, но еще не завершены)
//  - после старта опроса изменять его нельзя!

  def instructionExecutor(id: Int, in: List[String]): Unit = {
    in.map(CommandExecutor.parse(CommandExecutor.command(id), _))
  }
}
