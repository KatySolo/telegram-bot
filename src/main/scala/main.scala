import java.text.SimpleDateFormat
import java.util.Date

object main {
  def main(args: Array[String]): Unit = {
    file_manager.read().foreach(x => command_executor.parse(x))
  }
}