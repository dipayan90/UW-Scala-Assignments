package LambdaTest

import com.fortysevendeg.lambdatest._
import com.persist.uw.examples.FQueue

class LTestFQueue extends LambdaTest {

  val emptyQ = FQueue()

  def assertEmpty(q: FQueue): LambdaAct = {
    assertEq(q.size, 0, "check empty queue size") +
    assertEq(q.last, None, "check empty queue last") +
    assertEq(q.remove(), emptyQ, "check empty queue remove")
  }

  def act =
    label("Test FQueue") {
      val q = FQueue()
      test("init") {
        assertEmpty(q)
      } +
      test("insert") {
        val q1 = q.insert(10).insert(20).insert(30)
        assertEq(q1.size, 3, "insert 3") +
        test("remove") {
          val q2 = q1.remove
          val q3 = q2.remove
          assertEq(q1.last, Some(10), "first last") +
          assertEq(q2.last, Some(20), "second last") +
          assertEq(q3.last, Some(30), "third last") +
          test("final") {
            val q4 = q3.remove()
            assertEmpty(q4)
          }
        }
      }
    }
}

object LTestFQueue {
  def main(args: Array[String]): Unit = {
    run("ltfqueue", new LTestFQueue)
  }
}


