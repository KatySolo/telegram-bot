import info.mukel.telegrambot4s._
import api._
import info.mukel.telegrambot4s.api.declarative.{Commands, Help}
import methods._
import models._
import Implicits._
import info.mukel.telegrambot4s.api.declarative.Commands

object PollBot extends TelegramBot with Polling with Commands {
  def token = "614811011:AAFYpVpG5e_GJnkim22pwINOzqOHllD_Fbk"

  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text) {
      val responseText = CommandExecutor.parse(CommandExecutor.command(msg.from.get.id), text).get
//      val responseText = CommandExecutor.parse(text)
      request(SendMessage(msg.source, responseText))
    }
  }
}