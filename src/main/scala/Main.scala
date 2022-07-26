import scala.List
import scala.Nil

object Main {
  def main(args: Array[String]): Unit = {
    println(0x2500.toChar.toString * 45)

    val x = List(Some(1), Some(2), None, Some(3), Some(1))
    println(Option.sequence(x))

    println(0x2500.toChar.toString * 45)
  }
}