import scala.io.Source

val word = 
  Source.fromFile("input.txt")
        .mkString
        .split("\n\n")
        .map(_.split("\n"))

import scala.util.matching.Regex
val rangePattern = "(\\d+[-]\\d+)".r
val ourTicket = word(1)(1).split(",").map(_.toInt)
val tickets = word(2).tail.map(_.split(",").map(_.toInt))
val flattenedTickets = tickets.flatten
val listFilters = word(0).map(_.split(":")).map(x => (x(0), rangePattern.findAllIn(x(1)).toList.map(_.split("-").map(_.toInt))))
val rules = listFilters.map(_._2).flatten.toList

def someRuleWorks(r: List[Array[Int]], n: Int): Boolean = {
    r.count(applyRule(_, n)) > 0
}

def applyRule(r: Array[Int], n: Int): Boolean = {
    n >= r(0) & n <= r(1)
}

val validTickets = 
  tickets.filter(t => 
      t.count(n => someRuleWorks(rules, n)) == t.size
  )

val Rows = validTickets.transpose
val um = 
Rows.map(idx => 
         listFilters.map{case(row, r) => (row, idx.count(someRuleWorks(r, _)) == idx.size)})
 

def getFieldNames(eligible: Array[(Array[(String, Boolean)], Int)]): Array[(Int, String)] = {
    if(eligible.size == 0){
        return Array()
    }
    val itemz = eligible.sortBy(_._1.count(_._2))
    val g = itemz.head
    println(g._1.count(_._2))
    val field = g._1.filter(_._2)(0)._1
    
    Array((g._2, field)) ++ getFieldNames(itemz.tail.map{case(lst, idx) => (lst.filter(_._1 != field), idx)})
}

val q2 = getFieldNames(um.zipWithIndex)
           .filter(_._2.contains("departure"))
           .map(x => ourTicket(x._1).toLong)
           .product
