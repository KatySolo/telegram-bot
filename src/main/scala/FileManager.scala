import scala.io.Source.fromFile

object FileManager {
  def read(): Iterator[String] = fromFile("src/main/scala/input.txt").getLines
}