import java.text.SimpleDateFormat
import java.util.Date

object Main {
  def main(args: Array[String]): Unit = {
    FileManager.read().foreach(x => CommandExecutor.parse(x))
  }
}