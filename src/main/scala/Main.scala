object Main {
  def main(args: Array[String]): Unit = {
    println(0x2500.toChar.toString * 50)

    println(Stream.fibs.take(10).toList)

    println(0x2500.toChar.toString * 50)
  }
}
