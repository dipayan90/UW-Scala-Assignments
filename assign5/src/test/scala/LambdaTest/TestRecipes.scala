package LambdaTest

import com.fortysevendeg.lambdatest._
import _root_.LambdaTest.RecipeExample._
import com.persist.uw.examples.RecipeClasses._

import scala.collection.mutable.ListBuffer

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
      val allIngredients: List[Ingredient] = scrambledEggsIngredients ::: quicheIngredients
      r.house.buy(r.addToCart(allIngredients))
      assertEq(r.getWholeCost(allIngredients),1500,"cost of groceries")
    } +
    test("What ingredients do I have in my house?") {
      val atHome: List[Ingredient]  = r.house.getIngredients
      assert(atHome.nonEmpty)
    } +
    label("Preparing a quiche") {
      exec {
        // this should remove the ingredients used from the inventory
        val quicheIngredients : List[Ingredient] = r.quiche.ingredients
        System.out.println(r.house.ingredientsAtHome)
        //preparing quiche
        for(ing <- quicheIngredients){
          r.house.remove(ing)
        }
        System.out.println(r.house.ingredientsAtHome)
      }
    } +
    test("What ingredients do I have in my house?") {
      // After the quiche wa prepared
      val ingredientsInHouse = r.house.ingredientsAtHome.filter(_.pack!=0)
      val ingredientnames = ingredientsInHouse.map(_.name)
      assert(ingredientnames.contains("pepper"))
      assert(ingredientnames.contains("salt"))
      assert(ingredientnames.contains("butter"))
      assert(ingredientnames.contains("eggs"))
    } +
    test("What recipes in my cookbook use bacon?") {
      val recipes : List[Recipe] = r.cookBook.recipes
      val filteredList: List[Recipe] =  recipes.filter(_.ingredients.map(_.name).contains("bacon"))
      val result: List[String] =  filteredList.map(_.name)
      assert(result.contains("blt"))
      assert(result.contains("quiche"))
    } +
    test("How do I prepare a blt?") {
      val blt = r.bLT
      val actionNames : List[String] = blt.actions.map(_.name)
      assert(actionNames.contains("slice"))
      assert(actionNames.contains("cook"))
      assert(actionNames.contains("combine"))

      val ingredients : List[String] = blt.ingredients.map(_.name)
      assert(ingredients.contains("tomato"))
      assert(ingredients.contains("bacon"))
      assert(ingredients.contains("lettuce"))
    } +
    test("What recipes in my cookbook do I now have enough ingredients to prepare?") {
      val ingredientsLeft : List[Ingredient] = r.house.ingredientsAtHome.filter(_.pack!=0)
      val recipes: List[Recipe] = r.cookBook.recipes
      val result = new ListBuffer[String]
      for(rec <- recipes){
        if(r.canRecipeBeMade(rec,ingredientsLeft)){
         result += rec.name
        }
      }
      assert(result.contains("scrambledEggs"))
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
