//import org.scalatest.{FlatSpec, BeforeAndAfterEach}
//import CommandExecutor._
//
//class CommandExecutorTest extends FlatSpec with  BeforeAndAfterEach {
//
//  override def afterEach(): Unit = {
//    Polls = Map.empty
//  }
//
//  "Command Executor" should
//    "create a new poll" in {
//    parse("/create_poll my_poll_t1")
//    val result = Polls(1).name
//    assert(result == "my_poll_t1")
//  }
//
//  it should "give each poll unique ID" in {
//    parse("/create_poll my_poll_t2")
//    parse("/create_poll my_poll_t2")
//    val result = Polls(2).id
//    assert (result != Polls(3).id)
//  }
//
//  it should "have no effect after try to stop not started poll" in {
//    parse("/create_poll my_poll_t3")
//    parse("/stop_poll 1")
//    assert(!Polls(4).isRunning)
//
//  }
//
//  it should "stop started poll" in {
//    parse("/create_poll my_poll_t5")
//    parse("/start_poll 5")
//    assert(Polls(5).isRunning)
//    parse("/stop_poll 5")
//    assert(!Polls(5).isRunning)
//  }
//
//  it should "show all created polls" in {
//    parse("/create_poll my_poll_t2")
//    parse("/create_poll my_poll_t2")
//    assert (Polls.size == 2)
//  }
//
//  it should "show results of currently running poll" in {
//
//  }
//
//  it should "show results of finished polls" in {
//
//  }
//
//
//}
