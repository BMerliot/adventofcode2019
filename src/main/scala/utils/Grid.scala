package utils

class Grid[T](
          private val a: Array[Array[T]]
          ) {

  if (a.exists(line => line.length != width))
    throw new IllegalArgumentException("All line must have the same width")

  def height: Int = a.length
  def width: Int = if (a.length == 0) 0 else a(0).length

  def prettyPrint(sep: String = " ", boxSize: Int = a(0)(0).toString.length): Unit = {
    println(
      " " * (boxSize + sep.length) +
      (0 until width)
        .map(i => if (i < 10) " " + i else i)
        .mkString(" " * (boxSize + sep.length - 2))
    )
    a.zipWithIndex.foreach{
      case (line, i) =>
        println{
          (if (i < 10) " " + i else i) + sep +
          line.mkString(sep)
        }
    }
  }
}
