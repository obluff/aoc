import scala.io.Source

object main extends App {
    val file = 
      Source.fromFile("input.txt")
            .getLines
            .map(_.split("")) 
            .toArray
    
    def findTrees(slopes: Array[Array[String]])(dy: Int, dx: Int): Int = {
        val m = slopes.size - 1
        val n = slopes(0).size
        
        val iterations = m/dy
        (0 to iterations)
          .map(x => slopes((dy*x))((dx*x) % n))
          .count(_.head == '#')
    } 
    
    val result = 
      Seq((1,1), (3,1), (5,1), (7,1), (1,2))
        .map(x => findTrees(file)(x._2, x._1))
        .reduce(_*_)
    
}
