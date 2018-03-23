import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

case class Poll1(name: String, anonymous: Boolean, is_afterstop: Boolean, start: Date, end: Date, id: Int)

//val calendar = Calendar.getInstance()
//val start : Date= dateParser.parse("10:00:00 18:03:01")
//val end : Date = dateParser.parse("10:00:00 18:03:02")


val d = "name yes afterstop".split(" ")
val dateFormat = new SimpleDateFormat("hh:mm:ss yy:MM:dd")
val id = Stream.from(1).iterator

var Polls1: Map[Int, Poll1] = Map[Int, Poll1]()


create_poll(d)

def create_poll(str: Array[String]): Unit = {
  for {
    name <- str.lift(1)
    anon <- str.lift(2)
    as <- str.lift(3)
    start <- for(x <- str.lift(4)) yield dateFormat.parse(x)
    end <- for(x <- str.lift(5)) yield dateFormat.parse(x)
  } yield {
    val poll = Poll1(name, anon=="yes", as=="afterstop", start, end, id.next())
    Polls1 += poll.id -> poll
    println(s"Your poll id: ${poll.id}")
  }

}

Polls1.isEmpty


