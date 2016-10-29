package com.persist.uw.examples
import scala.annotation.tailrec

// make it similar to FQueue
// tests must pass
// use only immutable data
// if using recursion make sure its tail recursive
// use case classes and objects, not Scala collection types
// correctness not performance is the goal

object FSet {

  private case class NonEmptyFSet(i : Int,next: FSet) extends FSet

  private case object EmptyFSet extends FSet
    
  def apply(): FSet = EmptyFSet
}

sealed trait FSet {

  import FSet._

  def contains(i:Int): Boolean = {
    @tailrec def contains1(items : FSet): Boolean ={
      items match {
        case NonEmptyFSet(x,next) if x == i => true
        case NonEmptyFSet(y,next) if y != i => contains1(next)
        case _ => false
      }
    }
    contains1(this)
  }

  def contains(i:Int,items: FSet): Boolean = {
    @tailrec def contains1(items : FSet): Boolean ={
      items match {
        case NonEmptyFSet(x,next) if x == i => true
        case NonEmptyFSet(y,next) if y != i => contains1(next)
        case _ => false
      }
    }
    contains1(items)
  }

  def add(i: Int): FSet = if( this contains i) this else NonEmptyFSet(i,this)

  def delete(i: Int): FSet = if (this contains i ) {
    var newFSet = FSet()
   @tailrec def delete1(items: FSet) : FSet = {
      items match {
        case NonEmptyFSet(x,next) if x != i =>
        {
          newFSet = NonEmptyFSet(x,newFSet)
          delete1(next)
        }
        case _ => newFSet
      }
    }
    delete1(this)
    newFSet
  }else{
    this
  }

  def union(set1: FSet): FSet = {
    var newFSet = FSet()
    @tailrec def union1(items: FSet): FSet = {
      items match {
        case NonEmptyFSet(x,next) =>
          {
            if(!contains(x,newFSet)){
              newFSet = NonEmptyFSet(x,newFSet)
            }
            union1(next)
          }
        case _ => newFSet
      }
    }
    union1(this)
    union1(set1)
    newFSet
  }

  def intersect(set1: FSet): FSet = {
    var newFSet = FSet()
    @tailrec def intersect1(items: FSet): FSet = {
      items match {
        case NonEmptyFSet(x,next) =>
        {
          if(contains(x,set1)){
            newFSet = NonEmptyFSet(x,newFSet)
          }
          intersect1(next)
        }
        case _ => newFSet
      }
    }
    intersect1(this)
    newFSet
  }

  def subset(set1: FSet): Boolean = {
    var result = false
    @tailrec def subset1(items: FSet): Boolean ={
      items match {
        case NonEmptyFSet(x,next) if set1 contains x =>
        {
          result = true
          subset1(next)
        }
        case _=> false
      }
    }
    subset1(this)
    result
  }

  def equals(set1: FSet): Boolean = {
    var result = false
    @tailrec def equals1(items: FSet): Boolean ={
      items match {
        case NonEmptyFSet(x,next) if this contains x =>
        {
          result = true
          equals1(next)
        }
        case _=> false
      }
    }
    equals1(set1)
    result
  }

  def size: Int = {
    @tailrec def size1(items: FSet, accum: Int = 0): Int = {
      items match {
        case NonEmptyFSet(i, next) => size1(next, accum + 1)
        case EmptyFSet => accum
      }
    }
    size1(this)
  }
}
