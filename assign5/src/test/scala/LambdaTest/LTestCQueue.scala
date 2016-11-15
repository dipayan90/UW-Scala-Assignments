package LambdaTest

import com.fortysevendeg.lambdatest._
import com.persist.uw.examples.CQueue

class LTestCQueue extends LambdaTest {

  def assertEmpty(q: CQueue): LambdaAct = {
    assertEq(q.size, 0, "check empty queue size") +
    assertEq(q.remove, None, "check empty queue remove")
  }

  def act =
    label("Test CQueue") {
      val q = CQueue()
      test("init") {
        assertEmpty(q)
      } +
      test("insert") {
        q.insert(10)
        q.insert(20)
        q.insert(30)
        assertEq(q.size, 3, "inserted 3")
      } +
      test("remove") {
        assertEq(q.remove(), Some(10), "first remove") +
        assertEq(q.remove(), Some(20), "second remove") +
        assertEq(q.remove(), Some(30), "third remove")
      } +
      test("final") {
        assertEmpty(q)
      }
    }
}

object LTestCQueue {
  def main(args: Array[String]): Unit = {
    run("ltcqueue", new LTestCQueue)
  }
}

