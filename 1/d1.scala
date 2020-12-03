import scala.io.Source

object d1 extends App {

    val file = Source.fromFile("inp.txt")
                     .getLines
                     .map(_.toInt)
                     .toList

    def nSumProd(lst: List[Int], n: Int): Int = {
        lst.combinations(n)
           .find(_.sum == 2020)
           .get
           .reduceLeft(_ * _)
    }

    println(nSumProd(file, 2))
    println(nSumProd(file, 3))
}

