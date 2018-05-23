import java.text.SimpleDateFormat
import java.util.Date
import scala.io.Source.fromFile


object main {
  def main(args: Array[String]): Unit = {
//    file_manager.read().foreach(x => command_executor.parse(x))


    val map: Map[Int, Poll] = Map()

//    file_manager.read().foreach(x => command_executor.parse(command_executor.command, x))

    fromFile("src/main/scala/input.txt").getLines
      .map(command_executor.parse(command_executor.command, _).get)
      .foldLeft(map)((m, str) => {

//        println(str)
        m
      })

  }
}