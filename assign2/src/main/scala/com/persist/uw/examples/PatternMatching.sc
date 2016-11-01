val s = List((2,3),(4,5),(6,7))
val s1 = s.map(p => p._1 + p._2)
val s2 = s.map {case (a,b) => a + b }
val s3 = for ((a,b) <- s) yield a + b
val t = List(List(),List(1,2,3),List(4,5))
val heads = t.flatMap {
  case h +: t => Seq(h)
  case _ => Seq()
}
// h will be initially 1 and t is List(2,3) for List(1,2,3)
//then h will be 4 and t will be List(5) for List(4,5)
//result is list of heads List(1,4)
val fruits = List("100 bananas","200 apples")
val pattern = "([0-9]+) ([A-Za-z]+)".r
val fruits1 = fruits.map {
  case pattern(cnt,fruit) => (cnt.toInt,fruit)
  case _ => (0,???)
}
val fruits2 = fruits1.groupBy {case (cnt,f) => f}