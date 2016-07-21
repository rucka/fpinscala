package ch2
import scala.annotation.tailrec

object Exercise1 {
    def fib(n: Int) : Int = {
        @tailrec
        def loop(n: Int, prev: Int, curr: Int) : Int = n match {
            case 0 => prev
            case _ => loop(n - 1, curr, prev + curr)
        }
        loop(n, 0, 1)
    }
}

import Exercise1._

/*
from repl you can test typing:
    :load src/main/scala/fpinscala/ch2/Exercise1.scala
    Seq(0, 1, 2, 3, 4, 5) map fib
*/
