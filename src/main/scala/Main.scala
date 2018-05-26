import java.text.SimpleDateFormat
import java.util.Date
import scala.io.Source.fromFile

import info.mukel.telegrambot4s._


object main {
  def main(args: Array[String]): Unit = {
    PollBot.run()
//    file_manager.read().foreach(x => command_executor.parse(x))


//    val map: Map[Int, Poll] = Map()

//    file_manager.read().foreach(x => command_executor.parse(command_executor.command, x))

//    fromFile("src/main/scala/input.txt").getLines
//      .map(command_executor.parse(command_executor.command, _).get)
//      .foldLeft(map)((m, str) => {
////        val res = command_executor.parse_cmd(map, str)
////        m += 1 -> str
//        println(str)
////        println(m)
//        m
//      })
//      .foldLeft(map)(_+_)

  }
}