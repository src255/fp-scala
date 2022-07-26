object Main {
  def main(args: Array[String]): Unit = {
    println(0x2500.toChar.toString * 45)

    val x = List(Right(1), Right(2), Right(3), Left("Error"))
    println(Either.sequence(x))

    println(0x2500.toChar.toString * 45)
  }
}