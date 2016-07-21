package ch2
object Exercise4 {
    def uncurry[A,B,C](f: A => B => C) : (A, B) => C = (a, b) => f(a)(b)
}

import Exercise4._

/*
from repl you can test typing:
    :load src/main/scala/fpinscala/ch2/Exercise4.scala
    val add = (a: Int, b: Int) => a + b
    val curriedadd = (a : Int) => (b: Int) => (a + b)
    curriedadd(1)(2)
    uncurry(curriedadd)(1, 2)
*/
