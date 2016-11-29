package com.persist.uw.examples

object Simplify {

  // use pattern matching
  // no vars or mutable data
  // pass all tests

  def simplify(t: Tree3): Tree3 = {

    t match {
      case Add3(x,Int3(0)) => simplify(x)
      case Add3(Int3(0),x) => simplify(x)
      case Add3(Int3(x),Int3(y)) => Int3(x+y)
      case Add3(Multiply3(l1,r1),Multiply3(l2,r2)) if l1.equals(l2) => Multiply3(l1,simplify(Add3(r1,r2)))
      case Add3(left,right) =>  simplify(Add3(simplify(left),simplify(right)))

      case Subtract3(x,Int3(0)) => simplify(x)
      case Subtract3(left,right) if left.equals(right) => Int3(0)
      case Subtract3(Int3(x),Int3(y)) => Int3(x - y)
      case Subtract3(Multiply3(l1,r1),Multiply3(l2,r2)) if l1.equals(l2) => Multiply3(l1,simplify(Subtract3(r1,r2)))
      case Subtract3(left,right) => simplify(Subtract3(simplify(left),simplify(right)))

      case Multiply3(left,right) if left.equals(Int3(0)) || right.equals(Int3(0)) => Int3(0)
      case Multiply3(left,right) if left.equals(Int3(1)) => right
      case Multiply3(left,right) if right.equals(Int3(1)) => left
      case Multiply3(Int3(x),Int3(y)) => Int3(x * y)
      case Multiply3(left,right) => simplify(Multiply3(simplify(left),simplify(right)))

      case Divide3(left,right) if left.equals(right) => Int3(1)
      case Divide3(Name3(left),Name3(right)) => Divide3(Name3(left),Name3(right))
      case Divide3(Int3(x),Int3(y)) => Int3(x/y)
      case Divide3(left,right) => simplify(Divide3(simplify(left),simplify(right)))

      case If3(test,x,y) if test.equals(Int3(0)) => y
      case If3(test,x,y) if test.equals(Int3(1)) => x
      case If3(test,x,y) => simplify(If3(simplify(test),simplify(x),simplify(y)))
      case If3(test,Name3(x),Name3(y)) => If3(test,Name3(x),Name3(y))

      case _ => t
    }

  }
}

