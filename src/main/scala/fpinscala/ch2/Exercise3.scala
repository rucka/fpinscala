import scala.annotation.tailrec

object Exercise3 {
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
}

import Exercise3._

/*
from repl you can test typing: 
    :load src/main/scala/fpinscala/ch2/Exercise3.scala
    val add = (a: Int, b: Int) => a + b
    val incr = curry(add)(1)
    incr(2)
    add(1, 2)
*/
