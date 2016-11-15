package LambdaTest

import com.fortysevendeg.lambdatest._
import com.persist.uw.examples.FQueue
import org.scalacheck.Prop._

class SCTestFQueue extends LambdaTest {

  val emptyQ = FQueue()

  def add(q: FQueue, s: List[Int]): FQueue = {
    s.foldLeft(q) {
      (q1, i) => q1.insert(i)
    }
  }

  def remove(q: FQueue, cnt: Int): (FQueue, List[Int]) = {
    val (qa, sa) = (1 to cnt).foldLeft((q, List.empty[Int])) {
      case ((q1, s1), i) =>
        (q1.remove(), q1.last.get +: s1)
    }
    (qa, sa.reverse)
  }

  def act =
    test("ScalaCheck FQueue") {
      assertSC() {
        forAll { (s1: List[Int], s2: List[Int]) =>
          val q1 = add(add(emptyQ, s1), s2)
          val q2 = add(emptyQ, s2)
          val (q3, s3) = remove(q1, s1.length)
          q3 == q2 && s3 == s1
        }
      }
    }
}

object SCTestFQueue {
  def main(args: Array[String]): Unit = {
    run("scfqueue", new SCTestFQueue)
  }
}

