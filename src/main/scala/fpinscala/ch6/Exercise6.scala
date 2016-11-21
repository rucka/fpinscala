package ch6
import scala.annotation.tailrec
import RNG._

object Exercise6 {
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }
}

import Exercise6._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/Exercise6.scala
  map2(_.nextInt, _.nextInt)(_ + _)(Simple(1))
*/
