package LambdaTest

import com.persist.uw.examples.RecipeClasses._

case class RecipeExample() {

  import com.persist.uw.examples._

  // This class should define
  //
  // a: all ingredients (name, unit package size, warm/cold, cost)
  // b: all recipes and the steps in each with the ingredients used in each step
  // c: any other class instances you need

  val egg = Cold("eggs", "egg", 12, DollarsCents(3))
  val cream = Cold("cream", "T", 10, DollarsCents(2))
  val butter = Cold("butter", "T", 12, DollarsCents(2))
  val salt = Warm("salt", "pinch", 100, DollarsCents(1))
  val pepper = Warm("pepper", "pinch", 100, DollarsCents(1))
  val bacon = Cold("bacon", "strip", 30, DollarsCents(4))
  val swiss = Cold("swiss cheese", "slice", 20, DollarsCents(2))
  val lettuce = Cold("lettuce", "leaf", 30, DollarsCents(2))
  val tomatoe = Warm("tomatoe", "tomatoe", 1, DollarsCents(1))
  val bread = Warm("bread", "slice", 20, DollarsCents(2))

  def unitCost(ingredient: Ingredient): Int = {
    ingredient.price.cents / ingredient.pack
  }

  // house object to keep track of home
  val house = House

  //Scrambled Eggs
  /*
  Step 1. Whisk 3 eggs, 1 pinch salt, 1 pinch pepper
  Step 2. Cook the result of step 1 using 1 tablespoon of
          butter
  Step 3. Let the result of step 2 rest for 1 minute
   */
  val SEI1 = Cold("eggs", "egg", 3, Cost(unitCost(egg) * 3))
  val SEI2 = Warm("salt", "pinch", 1, Cost(unitCost(salt) * 1))
  val SEI3 = Warm("pepper", "pinch", 1, Cost(unitCost(pepper) * 1))
  val SEI4 = Cold("butter", "T", 1, Cost(unitCost(butter) * 1))
  val scrambledEggsIngredients = List(SEI1, SEI2, SEI3, SEI4)

  val SEA1 = Action("whisk", List(SEI1, SEI2, SEI3))
  val SEA2 = Action("cook", SEI4 :: SEA1.ingredientList)
  val SEA3 = Action("rest", SEA2.ingredientList, 1)
  val scrambledEggsActions = List(SEA1, SEA2, SEA3)
  val scrambledEggs = ScrambledEggs(scrambledEggsIngredients, scrambledEggsActions)

  //Quiche
  /*
  Step 1. Cook 3 slices of bacon
  Step 2. Shred 2 slides of swiss cheese
  Step 3. Combine the result of steps 1 and 2 with 3 eggs
          and 10 tablespoons of cream
  Step 4. Bake the result of step 3
  Step 5. Let the result of step 4 cool
   */

  val QI1 = Cold("bacon", "strip", 3, Cost(unitCost(bacon) * 3))
  val QI2 = Cold("swiss cheese", "slice", 2, Cost(unitCost(swiss) * 2))
  val QI3 = Cold("eggs", "egg", 3, Cost(unitCost(egg) * 3))
  val QI4 = Cold("cream", "T", 10, DollarsCents(2))
  val quicheIngredients = List(QI1, QI2, QI3, QI4)

  val QA1 = Action("cook", List(QI1))
  val QA2 = Action("shred", List(QI2))
  val QA3 = Action("combine", QI4 :: QA1.ingredientList ::: QA2.ingredientList)
  val QA4 = Action("bake", QA3.ingredientList)
  val QA5 = Action("cool", QA4.ingredientList)
  val quicheActions = List(QA1, QA2, QA3, QA4, QA5)
  val quiche = Quiche(quicheIngredients, quicheActions)

  //BLT
  /*
  Step1. Slice 1 tomato
  Step 2. Cook 3 slices of bacon
  Step 3. Combine the results of step 1 and 2 on 2 slices
          of bread with 2 leaves of lettuce
   */
  val BLTI1 = Warm("tomatoe", "tomatoe", 1, DollarsCents(1))
  val BLTI2 = Cold("bacon", "strip", 3, Cost(unitCost(bacon) * 3))
  val BLTI3 = Warm("bread", "slice", 2, Cost(unitCost(bread) * 2))
  val BLTI4 = Cold("lettuce", "leaf", 2, Cost(unitCost(lettuce) * 2))
  val bLTIngredients = List(BLTI1, BLTI2, BLTI3, BLTI4)

  val BLTA1 = Action("slice", List(BLTI1))
  val BLTA2 = Action("cook", List(BLTI2))
  val BLTA3 = Action("combine", List(BLTI3, BLTI4) ::: BLTA1.ingredientList ::: BLTA2.ingredientList)
  val bLTActions = List(BLTA1, BLTA2, BLTA3)
  val bLT = BLT(bLTIngredients, bLTActions)

}
