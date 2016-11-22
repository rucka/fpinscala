package ch6
import scala.annotation.tailrec
import RNG._

object Exercise7 {
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }
  def unit[A](a: A) = (r: RNG) => (a, r)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))
  }
  val int: Rand[Int] = _.nextInt
  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
}

import Exercise7._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/Exercise7.scala
  ints(Simple(5))
*/
