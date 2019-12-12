import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day07Part2 {
  def main(args: Array[String]) {

    // runProgram has been changed compared to part 1, to output the memory
    // AND a return code :
    // - 0 is continue
    // - 1 is output (and pause to let the next amplifier run)
    // - 99 is end
    @tailrec
    def runProgram(i: Int, arr: Array[Int])(
      inputQueue: mutable.Queue[Int],
      outputQueue: mutable.Queue[Int]
    ): (Array[Int], Int) = {

      def opProgram(f: (Int, Int) => Int)
                   (C: Int, B: Int, A: Int)
                   (i: Int, arr: Array[Int]): Unit = {
        val c: Int = if (C == 0) arr(i + 1) else i + 1;
        val b: Int = if (B == 0) arr(i + 2) else i + 2;
        val a: Int = if (A == 0) arr(i + 3) else i + 3;
        arr(a) = f(arr(c), arr(b))
      }

      def inputProgram(C: Int)(i: Int, arr: Array[Int]): Unit = {
        val inputInt: Int = inputQueue.dequeue
        if (C == 0) arr(arr(i + 1)) = inputInt else arr(i) = inputInt
      }

      def outputProgram(C: Int)(i: Int, arr: Array[Int]): Unit = {
        val message = if (C == 0) arr(arr(i + 1)) else arr(i + 1);
        outputQueue.enqueue(message)
      }

      def jumpProgram(f: Boolean => Boolean)
                     (C: Int, B: Int)
                     (i: Int, arr: Array[Int]): Int = {
        val c: Int = if (C == 0) arr(i + 1) else i + 1;
        val b: Int = if (B == 0) arr(i + 2) else i + 2;
        if (f(arr(c) != 0)) arr(b) - i else 3
      }

      implicit def bool2int(b: Boolean): Int = if (b) 1 else 0

      def read(i: Int): (Int, Int) = {
        val A: Int = arr(i) / 10000;
        val B: Int = (arr(i) / 1000) % 10;
        val C: Int = (arr(i) / 100) % 10;
        val DE: Int = arr(i) % 100;
        DE match {
          case 1 => opProgram(_+_)(C, B, A)(i, arr); (4, 0)
          case 2 => opProgram(_*_)(C, B, A)(i, arr); (4, 0)
          case 3 => inputProgram(C)(i, arr); (2, 0)
          case 4 => outputProgram(C)(i, arr); (2, 1)
          case 5 => (jumpProgram((b: Boolean) => b)(C, B)(i, arr), 0)
          case 6 => (jumpProgram((b: Boolean) => !b)(C, B)(i, arr), 0)
          case 7 => opProgram(_<_)(C, B, A)(i, arr); (4, 0)
          case 8 => opProgram(_==_)(C, B, A)(i, arr); (4, 0)
          case 99 => (0, 99)
          case _ => throw new Exception("Invalid instruction")
        }
      }

      read(i) match {
        case (_, 99) => (arr, 99)
        case (step, 1) => (arr, i + step)
        case (step, 0) => runProgram(i + step, arr)(inputQueue, outputQueue)
      }
    }

    def allCombinations(n: Int, m: Int): List[List[Int]] = {

      def combinations(l: List[Int], remainingSwaps: Int): List[List[Int]] = {
        if (remainingSwaps == 0) l :: Nil
        else l match {
          case h :: t =>
            combinations(t, t.size - 1).map((a: List[Int]) => h :: a).toList :::
              combinations(t :+ h, remainingSwaps - 1)
          case _ => Nil
        }
      }

      combinations((n to m).toList, m - n -1)
    }

    val source = Source.fromFile("src/main/resources/day07.txt")
    val inputArray = source.getLines
      .find(_ => true)
      .getOrElse("99")
      .split(",")
      .map((s: String) => s.toInt)

    def loopRunProgram(
                        memories: List[Array[Int]],
                        inputQueues: List[mutable.Queue[Int]],
                        outputQueues: List[mutable.Queue[Int]],
                        nextInstructions: List[Int],
                        lastOutput: Int
                      ): Int = {
      (memories, inputQueues, outputQueues, nextInstructions) match {
        case (mh :: mt, ih :: it, oh :: ot, nh :: nt) =>
          runProgram(nh, mh)(ih, oh) match {
            case (_, 99) => lastOutput
            case (mh, nextInstruction) => (it, oh.dequeue()) match {
              case (ihh :: itt, currentOutput) =>
                ihh.enqueue(currentOutput);
                loopRunProgram(
                  mt :+ mh,
                  (ihh :: itt) :+ ih,
                  ot :+ oh,
                  nt :+ nextInstruction,
                  currentOutput
                )
            }
          }
      }
    }

    def initInputs(l: List[Int]): List[mutable.Queue[Int]] = {
      l.map((i: Int) => {
          val q = mutable.Queue[Int]();
          q.enqueue(i);
          q
        }
      ).toList match {
        case h :: t =>
          h.enqueue(0);
          h :: t
      }
    }

    val part2: Int = allCombinations(5, 9)
      .map((l: List[Int]) => loopRunProgram(
        List.fill(l.size)(inputArray.clone()),
        initInputs(l),
        List.fill(l.size)(mutable.Queue[Int]()),
        List.fill(l.size)(0),
        0
      ))
      .max

    source.close()

    println(part2)
  }
}
