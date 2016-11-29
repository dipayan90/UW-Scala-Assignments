package LambdaTest

import com.fortysevendeg.lambdatest._
import com.persist.uw.examples._

class LTestTree1 extends LambdaTest {

  def generate(depth: Int): Tree1 = {
    if (depth == 1) {
      Int1(4)
    } else {
      Add1(generate(depth - 1), generate(1))
    }
  }

  def act = label("Test Tree1 Depth") {
    for (i <- 1 to 10) yield {
      test(s"Depth $i") {
        val t = generate(i)
        assertEq(t.depth, i)
      }
    }
  }
}

object LTestTree1 {
  def main(args: Array[String]): Unit = {
    run("lttree1", new LTestTree1)
  }
}


