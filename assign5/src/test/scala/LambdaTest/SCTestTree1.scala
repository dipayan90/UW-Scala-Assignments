package LambdaTest

import com.fortysevendeg.lambdatest._
import com.persist.uw.examples._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._

class SCTestTree1 extends LambdaTest {

  val genInt = for (i <- arbitrary[Int]) yield Int1(i)

  val genAdd = for {
    i <- arbitrary[Int] // To avoid ScalaCheck bug
    left <- genTree
    right <- genTree
  } yield Add1(left, right)

  val genTree: Gen[Tree1] = oneOf(genInt, genAdd)

  def down1(s: Set[Tree1]): Set[Tree1] = {
    s.flatMap { t =>
      t match {
        case Add1(left, right) => Set(left, right)
        case _ => Set.empty[Tree1]
      }
    }
  }

  def downN(s: Set[Tree1], n: Int): Set[Tree1] = {
    (1 to n).toSet.foldLeft(s) {
      case (s, i) => down1(s)
    }
  }

  def act =
    test("ScalaCheck Tree1 Depth") {
      assertSC() {
        forAll(genTree) { t =>
          val d = t.depth
          !downN(Set(t), d - 1).isEmpty && downN(Set(t), d).isEmpty
        }
      }
    }
}

object SCTestTree1 {
  def main(args: Array[String]): Unit = {
    run("sctree1", new SCTestTree1)
  }
}

