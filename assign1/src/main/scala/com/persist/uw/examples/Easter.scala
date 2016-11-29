package com.persist.uw.examples

// replace ??? 
// don't use any mutable data
// don't use var
// don't use return
// tests must pass

case class Day(month: Int, day: Int)

object Easter {

  def easter(year: Int): Day = {

    val c = year / 100
    val n = year - 19 * ( year / 19 )
    val k = ( c - 17 ) / 25
    val i = c - c / 4 - ( c - k ) / 3 + 19 * n + 15

    val a = i - 30 * ( i / 30 )
    val b = a - ( a / 28 ) * ( 1 - ( a / 28 ) * ( 29 / ( a + 1 ) )
      * ( ( 21 - n ) / 11 ) )

    val j = year + year / 4 + b + 2 - c + c / 4

    val p = j - 7 * ( j / 7 )
    val l = b - p
    val m = 3 + ( l + 40 ) / 44
    val d = l + 28 - 31 * ( m / 4 )
    val month = m
    val day = d

    Day(month, day)
  }

}
