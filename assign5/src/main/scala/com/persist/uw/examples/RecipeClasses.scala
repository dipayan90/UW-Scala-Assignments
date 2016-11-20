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
    var pack: Int
    val price: Cost
  }

  case class Warm(name: String, unit: String, var pack: Int, price: Cost) extends Ingredient

  case class Cold(name: String, unit: String, var pack: Int, price: Cost) extends Ingredient

  // Add the additional classes needed here

  case class Action(name: String, ingredientList: List[Ingredient],time: Int = 0)

  case class House() {

    var ingredientsAtHome = List.empty[Ingredient]

    def getIngredients: List[Ingredient] = ingredientsAtHome

    def buy(ingredients: List[Ingredient]) = {
      for(ing <- ingredients){
        if(checkContains(ing)){
          val filteredList = ingredientsAtHome.filter(_.name.equals(ing.name))
          filteredList.foreach(e => e.pack = e.pack+ing.pack)
        }else{
          ingredientsAtHome = ing :: ingredientsAtHome
        }
      }
    }

    def checkContains(ingredient: Ingredient): Boolean = {
      ingredientsAtHome.contains(ingredient)
    }

    def remove(ingredient: Ingredient) : Unit = {
      val filteredList = ingredientsAtHome.filter(_.name.equals(ingredient.name))
      filteredList.foreach(e => e.pack = e.pack-ingredient.pack)
    }

  }

  trait Recipe {
    val name: String
    val ingredients: List[Ingredient]
    val actions: List[Action]
  }

  case class ScrambledEggs(name: String = "scrambledEggs",ingredients: List[Ingredient],actions: List[Action]) extends Recipe
  case class BLT(name: String = "blt",ingredients: List[Ingredient],actions: List[Action]) extends Recipe
  case class Quiche(name :String = "quiche",ingredients: List[Ingredient],actions: List[Action]) extends Recipe
  case class CookBook(recipes: List[Recipe])
}
