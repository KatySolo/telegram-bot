import scala.io.Source.fromFile

object file_manager {
  def read(): Iterator[String] = fromFile("src/main/scala/input.txt").getLines
}