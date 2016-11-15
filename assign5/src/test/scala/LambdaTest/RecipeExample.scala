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

}
