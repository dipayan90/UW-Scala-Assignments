package LambdaTest

import com.fortysevendeg.lambdatest._
import LambdaTest.RecipeExample._
import com.persist.uw.examples.RecipeClasses._

// Replace each nyi and nop with needed code

class TestRecipes extends LambdaTest {

  def nyi: LambdaAct = assert(false, "not yet implemented")

  def nop: Unit = ()

  val r = new RecipeExample
  import r._

  def act =
    test("What is the cost of the ingredients in a blt?") {
      val cost = 171
      assertEq(r.recipeCost(r.bLT.ingredients),cost,"blt costs")
    } +
    test("What ingredients do I need to make a quiche?") {
      val QI1 = Cold("bacon", "strip", 3, Cost(unitCost(bacon) * 3))
      val QI2 = Cold("swiss cheese", "slice", 2, Cost(unitCost(swiss) * 2))
      val QI3 = Cold("eggs", "egg", 3, Cost(unitCost(egg) * 3))
      val QI4 = Cold("cream", "T", 10, DollarsCents(2))
      val quicheIngredients = List(QI1, QI2, QI3, QI4)
      assertEq(r.quiche.ingredients,quicheIngredients,"Quiche ingredients")
    } +
    test("What ingredients do I have in my house?") {
      val noIngredient = List.empty[Ingredient]
      assertEq(r.house.getIngredients,noIngredient,"ingredients at home")
    } +
    test("Buying everything I need to make scrambled eggs and a quiche") {
      // this is in addition to what is already in house
      // Note: you can only buy whole packages (so round up)
      // Report the cost of groceries purchased
      val scrambledEggsIngredients = r.scrambledEggs.ingredients
      val quicheIngredients = r.quiche.ingredients
      val allIngredients: Map[String,Int] = scrambledEggsIngredients ::: quicheIngredients groupBy (_.name) mapValues (_.map(_.pack).sum)
      allIngredients.to[List].foreach( _ match {
        case (k,v) if k.equals(r.pepper.name) => (k,math.ceil(r.pepper.pack % v) * r.pepper.pack,math.ceil(r.pepper.pack % v) * r.pepper.pack * r.pepper.price.cents)

      })
      System.out.println(allIngredients)
      nyi
    } +
    test("What ingredients do I have in my house?") {
      nyi
    } +
    label("Preparing a quiche") {
      exec {
        // this should remove the ingredients used from the inventory
        nop
      }
    } +
    test("What ingredients do I have in my house?") {
      // After the quiche wa prepared
      nyi
    } +
    test("What recipes in my cookbook use bacon?") {
      nyi
    } +
    test("How do I prepare a blt?") {
      nyi
    } +
    test("What recipes in my cookbook do I now have enough ingredients to prepare?") {
      nyi
    } +
    test("Going shopping") {
      // Set up shopping list with 10 tomatoes and 50 T butter
      // Only whole packages can be purchased
      // Add my purchases to my inventory
      // Report the cost of the purchase
      nyi
    } +
    test("What ingredients do I have in my house?") {
      // After shopping
      nyi
    }
}


object TestRecipes {
  def main(args: Array[String]): Unit = {
    run("recipes", new TestRecipes)
  }
}
