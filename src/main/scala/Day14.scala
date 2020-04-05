import scala.annotation.tailrec
import scala.io.Source

object Day14 {

  def loadReactions(): Map[String, (Int, Map[String, Int])] = {

    val source = Source.fromFile("src/main/resources/day14.txt")

    val reactions = source.getLines
      .map( (line: String) =>  {

        val splitProduct: Seq[String] = line.split("=>")

        val product: Seq[String] = splitProduct.last
          .trim
          .split(" ")
        val productName: String = product.last
        val productQuantity: Int = product.head.toInt

        val reactants: Seq[String] = splitProduct.head.split(",")
        val reactantsMap: Map[String, Int] = reactants
          .map(_.trim().split(" "))
          .map{a: Array[String] => a.last -> a.head.toInt}
          .toMap

        productName -> (productQuantity, reactantsMap)
      }).toMap

    source.close()
    reactions
  }

  def sortProducts(
                      reactions: Map[String, (Int, Map[String, Int])]
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
                             reactions: Map[String, (Int, Map[String, Int])],
                             productOrder: Seq[String]
                           ) {

    private def insert(
                queue: Seq[(String, Int)],
                product: (String, Int)
              ): Seq[(String, Int)] = {
      queue match {
        case Nil => product :: Nil
        case _ => if (productOrder.indexOf(queue.head._1) > productOrder.indexOf(product._1))
          product +: queue
        else queue.head +: insert(queue.tail, product)
      }
    }

    @tailrec
    private def updateQueue(
                     toProduce: Map[String, Int],
                     queue: Seq[(String, Int)]
                   ): Seq[(String, Int)] = {
      toProduce match {
        case m if m.isEmpty => queue
        case _ => {
          val h: (String, Int) = toProduce.head
          val t: Map[String, Int] = toProduce.tail
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
    private def howMuchOre(productionQueue: Seq[(String, Int)]): Int = {

      val (productName, productQuantity) = productionQueue.head
      reactions.get(productName) match {
        case None => productQuantity
        case Some((produced: Int, reactants: Map[String, Int])) =>
          val toProduce: Map[String, Int] = reactants.map{
            case (k, v) => (k, v * math.ceil(1.0 * productQuantity / produced).toInt)
          }
          val updatedProductionQueue: Seq[(String, Int)] =
            updateQueue(toProduce, productionQueue.tail)
          howMuchOre(updatedProductionQueue)
      }
    }

    def howMuchOre(fuelNeeded: Int = 1): Int =
      howMuchOre(("FUEL", fuelNeeded) :: Nil)

    @tailrec
    private def plan(
                      productionQueue: Seq[(String, Int)],
                      needs: Map[String, Int] = Map.empty,
                      scraps: Map[String, Int] = Map.empty
                    ): (Int, Map[String, Int], Map[String, Int]) = {

      val (productName, productQuantity) = productionQueue.head
      reactions.get(productName) match {
        case None => (productQuantity, needs, scraps)
        case Some((produced: Int, reactants: Map[String, Int])) => {

          val toProduce: Map[String, Int] = reactants.map {
            case (k, v) => (k, v * math.ceil(1.0 * productQuantity / produced).toInt)
          }
          val updatedProductionQueue: Seq[(String, Int)] =
            updateQueue(toProduce, productionQueue.tail)

          val newNeeds: Map[String, Int] =
            toProduce.filterNot{case (name, _) => needs.contains(name)}
          val updatedNeeds: Map[String, Int] = needs.map{
            case (name, quantity) =>
              if (toProduce.contains(name)) (name, quantity + toProduce(name))
              else (name, quantity)
          } ++ newNeeds

          val scrap: Int = productQuantity % produced

          if (scrap == 0)
            plan(updatedProductionQueue, updatedNeeds, scraps)

          else {

            val updatedScraps: Map[String, Int] =
              if (scraps.contains(productName))
                scraps.updated(productName, scraps(productName) + scrap)
              else
                scraps.updated(productName, scrap)

            plan(updatedProductionQueue, updatedNeeds, updatedScraps)
          }
        }
      }
    }

    @tailrec
    private def groupByReaction( // Actually grouped by reaction and merged if some reactions have common reactants
                                 reactants: Seq[String],
                                 result: Seq[Seq[String]] = Nil
                               ): Seq[Seq[String]] = {
      reactants match {
        case Nil => result
        case h :: t => {

          val otherReactants: Seq[String] = reactions.map {
            t => t._2._2.keys // take only the reactant names in each reaction
          }
            .filter(_.toSeq.contains(h)) // take only reactions containing h
            .flatten.toSeq
            .filter{ s => result.exists(_.contains(s)) } // take all reactants both in reactions with h and i result

          val updatedResult: Seq[Seq[String]] = otherReactants match {
            case Nil => (h :: Nil) +: result
            case _ => result.groupBy {
              g => g.exists(otherReactants.contains(_))
            }.flatMap {
              case (true, groups) => (h +: groups.flatten) :: Nil
              case (false, groups) => groups
            }.toSeq
          }

          groupByReaction(t, updatedResult)
        }
      }
    }

    def howMuchFuel(availableOre: Long): Long = {

      val (oneFuelinOre: Int, needs: Map[String, Int], scraps: Map[String, Int]) =
        plan(("FUEL", 1) :: Nil)

      val groupedScraps: Seq[Map[String, Int]] = groupByReaction(scraps.keys.toSeq)
        .map{
          group => group.map{
            name: String => name -> scraps(name)
          }.toMap
        }
      println(groupedScraps.foreach(println))

      val scrapsLcm: Long = groupedScraps.map{
        _.values
          .map(_.toLong)
          .foldLeft(1L)(lcm)
      }
        .foldLeft(1L)(lcm)

      val scrapLoops: Long = availableOre / (scrapsLcm * oneFuelinOre)
      val fuelCreatedInLoop: Long = scrapLoops * scrapsLcm + 1
      val remainingOre: Long = availableOre % (scrapsLcm * oneFuelinOre)

      val (remainingFuel: Int, remainingNeeds: Map[String, Int], remainingScraps: Map[String, Int]) =
        plan(("FUEL", (remainingOre / oneFuelinOre).toInt) :: Nil)

      remainingScraps.toSeq.sortBy{t => productOrder.indexOf(t._1)}.foreach(println)
      println()
      println(String.format("remaining ORE : %d, remaining needs: %d ", remainingOre, remainingNeeds("ORE")))

      fuelCreatedInLoop + remainingFuel
    }
  }

  def gcd(a: Long, b: Long): Long = {
    if (a == 0) b
    else if (b == 0) a
    else gcd(b, a % b)
  }

  def lcm(a: Long, b: Long): Long = Math.abs((a / gcd(a, b)) * b)

  def main(args: Array[String]): Unit = {

    val reactions: Map[String, (Int, Map[String, Int])] = loadReactions()
    val productOrder: Seq[String] = sortProducts(reactions)
    val nanofactoryComputer: NanofactoryComputer =
      NanofactoryComputer(reactions, productOrder)

    val part1: Int = nanofactoryComputer.howMuchOre(1)
    println(part1)

    val part2: Long = nanofactoryComputer.howMuchFuel(1000000000000L)
    println(part2)
  }
}
