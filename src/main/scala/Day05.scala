import scala.annotation.tailrec
import scala.io.Source
import scala.runtime.ScalaRunTime.stringOf

object Day05 {
  def main(args: Array[String]) {

    @tailrec
    def runProgram(i: Int, arr: Array[Int])(inputInt: Int): Array[Int] = {

      def opProgram(f: (Int, Int) => Int)(C: Int, B: Int, A: Int)(i: Int, arr: Array[Int]): Unit = {
        val c: Int = if (C == 0) arr(i + 1) else i + 1;
        val b: Int = if (B == 0) arr(i + 2) else i + 2;
        val a: Int = if (A == 0) arr(i + 3) else i + 3;
        arr(a) = f(arr(c), arr(b))
      }

      def inputProgram(C: Int)(i: Int, arr: Array[Int]): Unit = {
        if (C == 0) arr(arr(i + 1)) = inputInt else arr(i) = inputInt
      }

      def outputProgram(C: Int)(i: Int, arr: Array[Int]): Unit = {
        val message = if (C == 0) arr(arr(i + 1)) else arr(i + 1);
        println(message)
      }

      def jumpProgram(f: Boolean => Boolean)(C: Int, B: Int)(i: Int, arr: Array[Int]): Int = {
        val c: Int = if (C == 0) arr(i + 1) else i + 1;
        val b: Int = if (B == 0) arr(i + 2) else i + 2;
        if (f(arr(c) != 0)) arr(b) - i else 3
      }

      implicit def bool2int(b:Boolean): Int = if (b) 1 else 0

      def read(i: Int): Int = {
        val A: Int = arr(i) / 10000;
        val B: Int = (arr(i) / 1000) % 10;
        val C: Int = (arr(i) / 100) % 10;
        val DE: Int = arr(i) % 100;
        DE match {
          case 1 => opProgram(_+_)(C, B, A)(i, arr); 4
          case 2 => opProgram(_*_)(C, B, A)(i, arr); 4
          case 3 => inputProgram(C)(i, arr); 2
          case 4 => outputProgram(C)(i, arr); 2
          case 5 => jumpProgram((b: Boolean) => b)(C, B)(i, arr)
          case 6 => jumpProgram((b: Boolean) => !b)(C, B)(i, arr)
          case 7 => opProgram(_<_)(C, B, A)(i, arr); 4
          case 8 => opProgram(_==_)(C, B, A)(i, arr); 4
          case 99 => 0
          case _ => throw new Exception("Invalid instruction")
        }
      }

      read(i) match {
        case 0 => arr
        case step => runProgram(i + step, arr)(inputInt)
      }
    }

    val source = Source.fromFile("src/main/resources/day05.txt")
    val inputArray = source.getLines
      .find(_ => true)
      .getOrElse("99")
      .split(",")
      .map((s: String) => s.toInt)

    val part1: Array[Int] = runProgram(0, inputArray.clone)(1)
    val part2: Array[Int] = runProgram(0, inputArray.clone)(5)

  }
}
