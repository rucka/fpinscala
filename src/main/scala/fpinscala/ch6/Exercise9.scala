package ch6
import scala.annotation.tailrec
import RNG._

object Exercise9 {
  def unit[A](a: A) = (r: RNG) => (a, r)
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {rng =>
    val (a, r) = f(rng)
    g(a)(r)
  }
  def map[A,B](a: Rand[A])(f: (A) => B): Rand[B] = flatMap(a) {rng =>
    unit(f(rng))
  }
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

import Exercise9._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/Exercise9.scala
*/
