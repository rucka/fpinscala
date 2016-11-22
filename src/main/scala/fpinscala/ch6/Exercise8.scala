package ch6
import scala.annotation.tailrec
import RNG._

object Exercise8 {
  def unit[A](a: A) = (r: RNG) => (a, r)
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {rng =>
    val (a, r) = f(rng)
    g(a)(r)
  }

  val nonNegativeInt = map(int)(i => if (i < 0) -(i + 1) else i)
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }
}

import Exercise8._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/Exercise8.scala
  nonNegativeLessThan(5)(Seed(1))
*/
