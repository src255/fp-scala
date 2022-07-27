object Main {
  def main(args: Array[String]): Unit = {
    println(0x2500.toChar.toString * 50)

    println(Stream(1, 2, 3).tails.map(_.toList).toList)

    println(0x2500.toChar.toString * 50)
  }
}
