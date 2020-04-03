import scala.io.Source

object IntCodeMachine {

  def load(fileName: String): Map[BigInt, BigInt] = {
    val source: Source = Source.fromFile(fileName)
    val mem: Map[BigInt, BigInt] = source
      .getLines.next()
      .split(",")
      .zipWithIndex
      .map((t: (String, Int)) => (BigInt(t._2), BigInt(t._1)))
      .toMap.withDefaultValue(BigInt(0))
    source.close()
    mem
  }

  def run(machine: IntCodeMachine): IntCodeMachine = {
    machine.next() match {
      case same if (same.equals(machine)) => machine
      case nextMachine => run(nextMachine)
    }
  }
}

class IntCodeMachine(
                          val mem: Map[BigInt, BigInt],
                          val i: BigInt = 0,
                          val machineInput: Seq[BigInt] = Nil,
                          val machineOutput: Seq[BigInt] = Nil
                    ) {

  // Parameter modes as described on day 5
  val c: BigInt = pointer(i + 1, (mem(i) / 100) % 10)
  val b: BigInt = pointer(i + 2, (mem(i) / 1000) % 10)
  val a: BigInt = pointer(i + 3, mem(i) / 10000)

  private def pointer(value: BigInt, mode: BigInt): BigInt =
    mode.toInt match {
      case 0 => mem(value)
      case 1 => value
      case 2 => mem(value) + mem(-1)
    }

  def run(): IntCodeMachine = IntCodeMachine.run(this)

  def next(): IntCodeMachine = {

    (mem(i) % 100).toInt match {
      case 1 => operator(_+_)
      case 2 => operator(_*_)
      case 3 => readInput()
      case 4 => writeOutput()
      case 5 => jump((b: Boolean) => !b)
      case 6 => jump((b: Boolean) => b)
      case 7 => operator(_<_)
      case 8 => operator(_.equals(_))
      case 9 => setRelativeBase()
      case 99 => this
      case _ => throw new Exception("Invalid instruction")
    }
  }

  protected def operator(f: (BigInt, BigInt) => BigInt): IntCodeMachine =
    new IntCodeMachine(
      mem + (a -> f(mem(c), mem(b))), i+4, machineInput, machineOutput
    )

  protected def readInput(): IntCodeMachine =
    new IntCodeMachine(
      mem + (c -> machineInput.head), i+2, machineInput.tail, machineOutput
    )

  protected def writeOutput(): IntCodeMachine = {
    new IntCodeMachine(
      mem, i+2, machineInput, machineOutput :+ mem(c)
    )
  }


  protected def jump(f: Boolean => Boolean): IntCodeMachine =
    new IntCodeMachine(
      mem,
      if (f(mem(c).equals(BigInt(0)))) mem(b) else i+3,
      machineInput,
      machineOutput
    )

  protected def setRelativeBase(): IntCodeMachine =
    new IntCodeMachine(
      mem + (BigInt(-1) -> (mem(-1) + mem(c))), i+2, machineInput, machineOutput
    )

  private implicit def bool2bigint(b: Boolean): BigInt = if (b) 1 else 0

  def copy(
            mem: Map[BigInt, BigInt] = this.mem,
            i: BigInt = this.i,
            machineInput: Seq[BigInt] = this.machineInput,
            machineOutput: Seq[BigInt] = this.machineOutput
          ): IntCodeMachine = {
    new IntCodeMachine(mem, i, machineInput, machineOutput)
  }
}
