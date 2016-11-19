package com.persist.uw.examples

object RecipeClasses {

  def DollarsCents(dollars: Int = 0, cents: Int = 0) = Cost(dollars * 100 + cents)

  case class Cost(cents: Int) {
    def *(i: Int) = Cost(i * cents)

    def /(i: Int) = Cost((cents + i - 1) / i)

    private def D = "$"

    override def toString: String = f"$D${cents / 100}.${cents % 100}%02d"
  }


  trait Ingredient {
    val name: String
    val unit: String
    val pack: Int
    val price: Cost
  }

  case class Warm(name: String, unit: String, pack: Int, price: Cost) extends Ingredient

  case class Cold(name: String, unit: String, pack: Int, price: Cost) extends Ingredient

  // Add the additional classes needed here

  case class Action(name: String,ingredientList: List[Ingredient]) {
    val home = new House

  }

   class House(){

    val ingredientsAtHome = List.empty[Ingredient]

    def getIngredients: List[Ingredient] =  ingredientsAtHome

    def buy(ingredient: Ingredient) = {
       ingredient :: ingredientsAtHome
    }

     def checkAvailability(ingredient: Ingredient) : Int = {
       ingredientsAtHome.filter(e => e.equals(ingredient)).map(i => i.pack).sum
     }

  }

}
