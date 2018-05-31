import java.text.SimpleDateFormat
import java.util.{Calendar, Date}


implicit val ex: Int = 6
def a(r:Int)(implicit ex: Int): Int = {
  ex
}


a(4)(5)
ex