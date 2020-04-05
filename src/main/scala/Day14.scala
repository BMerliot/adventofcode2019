import scala.annotation.tailrec
import scala.io.Source

object Day14 {

  def loadReactions(): Map[String, (Long, Map[String, Long])] = {

    val source = Source.fromFile("src/main/resources/day14.txt")

    val reactions = source.getLines
      .map( (line: String) =>  {

        val splitProduct: Seq[String] = line.split("=>")

        val product: Seq[String] = splitProduct.last
          .trim
          .split(" ")
        val productName: String = product.last
        val productQuantity: Long = product.head.toLong

        val reactants: Seq[String] = splitProduct.head.split(",")
        val reactantsMap: Map[String, Long] = reactants
          .map(_.trim().split(" "))
          .map{a: Array[String] => a.last -> a.head.toLong}
          .toMap

        productName -> (productQuantity, reactantsMap)
      }).toMap

    source.close()
    reactions
  }

  def sortProducts(
                      reactions: Map[String, (Long, Map[String, Long])]
                    ): Seq[String] = {

    def isReactant(reactant: String, product: String): Boolean = {
      reactions.get(product) match {
        case Some(t) => t._2.exists{
          case (subProduct, _) =>
            subProduct.equals(reactant)|| isReactant(reactant, subProduct)
        }
        case None => false
      }
    }

    def insert(sortedNames: Seq[String], product: String): Seq[String] = {
      sortedNames.indexWhere(isReactant(_, product)) match {
        case -1 => sortedNames :+ product
        case i => {
          val (front, back) = sortedNames.splitAt(i)
          front ++ List(product) ++ back
        }
      }
    }

    val sortedProducts: Seq[String] = reactions.keys.foldLeft(Nil: Seq[String])(insert)
    insert(sortedProducts, "ORE")
  }

  case class NanofactoryComputer(
                             reactions: Map[String, (Long, Map[String, Long])],
                             productOrder: Seq[String]
                           ) {

    private def insert(
                queue: Seq[(String, Long)],
                product: (String, Long)
              ): Seq[(String, Long)] = {
      queue match {
        case Nil => product :: Nil
        case _ => if (productOrder.indexOf(queue.head._1) > productOrder.indexOf(product._1))
          product +: queue
        else queue.head +: insert(queue.tail, product)
      }
    }

    @tailrec
    private def updateQueue(
                     toProduce: Map[String, Long],
                     queue: Seq[(String, Long)]
                   ): Seq[(String, Long)] = {
      toProduce match {
        case m if m.isEmpty => queue
        case _ => {
          val h: (String, Long) = toProduce.head
          val t: Map[String, Long] = toProduce.tail
          queue.indexWhere(_._1.equals(h._1)) match {
            case -1 =>
              updateQueue(t, insert(queue, h))
            case i =>
              updateQueue(t, queue.updated(i, (h._1, queue(i)._2 + h._2)))
          }
        }
      }
    }

    @tailrec
    private def howMuchOre(productionQueue: Seq[(String, Long)]): Long = {

      val (productName, productQuantity) = productionQueue.head
      reactions.get(productName) match {
        case None => productQuantity
        case Some((produced: Long, reactants: Map[String, Long])) =>
          val toProduce: Map[String, Long] = reactants.map{
            case (k, v) => (k, v * math.ceil(1.0 * productQuantity / produced).toLong)
          }
          val updatedProductionQueue: Seq[(String, Long)] =
            updateQueue(toProduce, productionQueue.tail)
          howMuchOre(updatedProductionQueue)
      }
    }

    def howMuchOre(fuelNeeded: Long = 1): Long =
      howMuchOre(("FUEL", fuelNeeded) :: Nil)

    def howMuchFuel(
                     availableOre: Long,
                     lowFuel: Long,
                     highFuel: Long
                   ): Long = {
      val midFuel: Long = (lowFuel + highFuel) / 2
      if (midFuel == lowFuel) lowFuel
      else {
          val oreNeeded: Long = howMuchOre(midFuel)
          if (oreNeeded > availableOre)
            howMuchFuel(availableOre, lowFuel, midFuel)
          else
            howMuchFuel(availableOre, midFuel, highFuel)
      }
    }

    def howMuchFuel(availableOre: Long): Long = {
      // Had to check someone else answer, did not think a simple dichotomy would be efficient enough
      howMuchFuel(availableOre, availableOre / howMuchOre(), availableOre / howMuchOre() * 2)
    }
  }

  def gcd(a: Long, b: Long): Long = {
    if (a == 0) b
    else if (b == 0) a
    else gcd(b, a % b)
  }

  def lcm(a: Long, b: Long): Long = Math.abs((a / gcd(a, b)) * b)

  def main(args: Array[String]): Unit = {

    val reactions: Map[String, (Long, Map[String, Long])] = loadReactions()
    val productOrder: Seq[String] = sortProducts(reactions)
    val nanofactoryComputer: NanofactoryComputer =
      NanofactoryComputer(reactions, productOrder)

    val part1: Long = nanofactoryComputer.howMuchOre(1)
    println(part1)

    val part2: Long = nanofactoryComputer.howMuchFuel(1000000000000L)
    println(part2)
  }
}
