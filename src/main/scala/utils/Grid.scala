package utils

class Grid[T](
    private val a: Array[Array[T]]
) {

  if (a.exists(line => line.length != width))
    throw new IllegalArgumentException("All line must have the same width")

  def apply(i: Int, j: Int): T = a(i)(j)
  def apply(ij: (Int, Int)): T = a(ij._1)(ij._2)

  def height: Int = a.length
  def width: Int = if (a.length == 0) 0 else a(0).length

  def getArray: Array[Array[T]] = a

  def prettyPrint(
      sep: String = " ",
      boxSize: Int = a(0)(0).toString.length
  ): Unit = {
    println(
      " " * (boxSize + sep.length) +
        (0 until width)
          .map(i => if (i < 10) " " + i else i)
          .mkString(" " * (boxSize + sep.length - 2))
    )
    a.zipWithIndex.foreach {
      case (line, i) =>
        println {
          (if (i < 10) " " + i else i) + sep +
            line.mkString(sep)
        }
    }
  }

  def find(t: T): Seq[(Int, Int)] = {
    a.zipWithIndex
      .flatMap {
        case (line, i) =>
          line.zipWithIndex
            .filter(_._1 == t)
            .map { case (_, j) => (i, j) }
          .toSeq
      }
  }
}
