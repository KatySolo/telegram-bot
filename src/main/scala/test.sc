import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.util.Try
var Polls: Map[Int, List[Int]] = Map.empty

implicit val ex: Int = 6
def a(r:Int)(implicit ex: Int): Int = {
  ex
}

Polls += 1 -> (1 :: Polls.getOrElse(1, List.empty))
Polls += 1 -> (2 :: Polls(1))


Polls



