import java.text.SimpleDateFormat
import java.util.Date
import scala.io.Source.fromFile

import info.mukel.telegrambot4s._


object main {
  def main(args: Array[String]): Unit = {
//    PollBot.run()

    val map: Map[Int, Poll] = Map()

//    FileManager.read().foreach(x => CommandExecutor.parse(CommandExecutor.command, x))

    FileManager.read()
      .map(CommandExecutor.parse(CommandExecutor.command(5), _).get)
      .foldLeft(map)((m, str) => {
        println(str)
//        println(m)
        m
      })
      .foldLeft(map)(_+_)

  }
}