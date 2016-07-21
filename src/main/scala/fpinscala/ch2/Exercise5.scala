package ch2
object Exercise5 {
    def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}

import Exercise5._

/*
from repl you can test typing:
    :load src/main/scala/fpinscala/ch2/Exercise5.scala
    val inc = (a: Int) => a + 1
    val sqr = (a: Int) => a * a

    sqr(inc(2))
    compose(sqr, inc)(2)
*/
