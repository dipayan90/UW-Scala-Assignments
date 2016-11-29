package com.persist.uw.examples

import scala.collection.breakOut
import scala.collection.mutable.{ListBuffer, Queue}

case class State(contains: List[Int]) {
  override def toString = contains.mkString("[", ",", "]")
}

case class Water(end: State, sizes: State) {

  type Path = List[State]

  case class Container(name: String,capacity: Int)

  type StateStatus = Map[Container,Int]

  type Move = (Container,Container)

  def solve(start: State): Option[Path] = {
    val parentMap = BFS(start)
    getPath(parentMap)
  }

  def getPath(parentMap : Map[State,State]) : Option[Path] = {
    // If element doesn't exist, we will not have a path
    if(!parentMap.contains(end)){
      return None
    }
    var current = parentMap.get(end).get
    val result = new ListBuffer[State]
    while(current != null){
      result += current
      current = parentMap.get(current).get
    }
    Some(result.toList)
  }

  def BFS(start: State) : Map[State,State] = {
    var alreadyVisited = Set.empty[State]
    var toVisit = Queue.empty[State]
    var parentMap = Map.empty[State,State]

    toVisit += start
    parentMap += start -> null

    while(toVisit.nonEmpty){
      val current = toVisit.dequeue()
      val currentStatus: Map[Container,Int] = generateStateStatus(current)
      alreadyVisited += current
      if(current.equals(end)){
        // Once we get the solution then return
        return parentMap
      }else{
        val moves: List[Move] = possibleMoves(currentStatus)
        val childStateStatus : List[StateStatus] =  moves.map(m => applyMove(currentStatus,m))
        val children: List[State] =  childStateStatus.map(s => getStateFromStatus(s))
        children.map(c => if(!alreadyVisited.contains(c)) {  parentMap += c -> current; toVisit += c})
      }
    }
    parentMap
  }

  def generateStateStatus(currentState: State): Map[Container,Int] ={
    val containers : List[Container] = sizes.contains.map(s => new Container(s.toString+math.random.toString,s))
    val status: Map[Container,Int] =  (containers zip currentState.contains)(breakOut)
    status
  }

  def getStateFromStatus(stateStatus: StateStatus) : State = {
    val sortedMap : Seq[(Container, Int)] = stateStatus.toSeq.sortBy(_._1.capacity)
    val res: List[Int] =  sortedMap.map(_._2).toList
    State(res)
  }

  def possibleMoves(state: StateStatus): List[Move] = {
    val containers: List[Container] = state.keys.toList
    for {
      s <- containers
      t <- containers
    //1. Can't go to same container
    //2. from cannot be 0
    //3. to cannot be full
      if (s != t && state(s) != 0 && state(t) != t.capacity)
    } yield (s, t)
  }

  def applyMove(state: StateStatus, move: Move): StateStatus = {
    val from = move._1
    val to = move._2
    // amount to pour is to - (if anything is already in to )
    val remainingCapacity = to.capacity - state(to)
    val transfer = math.min(state(from), remainingCapacity)
    // if there is nothing to be transfered return same map
    if (transfer == 0) state
      // from becomes state of from - amount to transfer
      // to becomes state of to + amount to transfer
    else state ++ List(from -> (state(from) - transfer), to -> (state(to) + transfer))
  }




}

object WaterTest {

  case class Jar(size: Int, start: Int, end: Int)

  def test(name: String, jars: List[Jar]): Unit = {
    val sizes = State(jars.map(_.size))
    val start = State(jars.map(_.start))
    val end = State(jars.map(_.end))
    val water = Water(end, sizes)
    val result = water.solve(start)
    println(s"*** $name $sizes ***")
    result match {
      case None => println("No Solution")
      case Some(r) =>
        if (r.size > 25) {
          println(s"Too many steps ${r.size}")
        } else
          for ((s, i) <- r.zipWithIndex) println(s"${i + 1} $s")
    }
  }

  def main(args: Array[String]): Unit = {
    val jars1 = List(Jar(3, 0, 0), Jar(5, 0, 4), Jar(8, 8, 4))
    val jars2 = List(Jar(5, 0, 0), Jar(11, 0, 8), Jar(13, 0, 8), Jar(24, 24, 8))
    val jars3 = List(Jar(4, 0, 3), Jar(5, 0, 3), Jar(10, 10, 4), Jar(10, 10, 10))
    val jars4 = List(Jar(2, 0, 2), Jar(2, 0, 2), Jar(100, 0, 48), Jar(100, 100, 48))
    val jars5 = List(Jar(2, 0, 1), Jar(2, 0, 1), Jar(100, 0, 49), Jar(100, 100, 49))
    test("Test1", jars1)
    test("Test2", jars2)
    test("Test3", jars3)
    test("Test4", jars4)
    test("Test5", jars5)
  }
}
