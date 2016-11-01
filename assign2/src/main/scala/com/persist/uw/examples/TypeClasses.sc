val l0 = List[Int]()
val l1 = List.empty[Int]
val l2 = List(1,2,3)

val a1 = List(l1,l2)
val a2 = a1.map(_.headOption)

//there is no default tail option

//we added tail option method to List collection
// using an implicit class below

l1.tailOption
l2.tailOption
trait HasTailOption[T] {
  def tailOption: Option[T]
}

implicit class ExtendList(l:List[Int]) extends HasTailOption[List[Int]]{
  def tailOption = {
    l match {
      case h +: t => Some(t)
      case _ => None
    }
  }
}