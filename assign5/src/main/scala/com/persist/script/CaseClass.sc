case class Person(name: String, age: Int = 20)

object Person1 {
  def apply(name: String, age: Int = 20) = new Person1(name, age)

  def unapply(p: Person1): Option[(String, Int)] = Some(p.name, p.age)
}

class Person1(val name: String, val age: Int) extends Product2[String, Int] {
  override def toString() = s"Person($name,$age)"

  override def equals(other: Any): Boolean = {
    other match {
      case Person1(name1, age1) =>
        name == name1 && age == age1
      case _ => false
    }
  }

  def copy(name: String = this.name, age: Int = this.age) = Person1(name, age)

  override def hashCode() = (name, age).##

  override def _1: String = name

  override def _2: Int = age

  override def canEqual(that: Any): Boolean = true
}

val p = Person1("Jim", 23)
val q = Person1("Jim", 23)
val Person1(n, a) = p
val r = p.copy(age = p.age + 1)

val pa = p.productArity
val n1 = p._1
val n2 = p._2
