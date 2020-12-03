import scala.io.Source

object d2 extends App {
    def boolToInt(x: Boolean): Int = {
       if(x == true) return 1 else 0
    }
    val file = Source.fromFile("input.txt")
                         .getLines
                         .toArray
    val pattern = "(\\d+)-(\\d+) ([a-zA-Z]): ([A-Za-z]+)".r
    val result = 
      file.map{ line =>  
        line match {
           case pattern(lb, ub, l, ls) => {
           //UGLY
             val ch = l.head
             val count = ls.count(_ == ch)
             val q1 = boolToInt(((lb.toInt <= count) && (count <= ub.toInt)))
             val left = boolToInt(ls(lb.toInt-1) == ch)
             val right = boolToInt(ls(ub.toInt-1) == ch)
             val result = boolToInt((left + right) == 1)
             (q1, result)
           }
           case _ => (0, 0)
          }
      }.reduceLeft((x, y) => (x._1 + y._1, x._2 + y._2))
}

