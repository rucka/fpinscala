package ch6
import scala.annotation.tailrec
import RNG._

object Exercise4 {
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec def loop(c: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (c <= 0) (xs, r)
      else {
        val (x, r2) = r.nextInt
        loop(c - 1, r2, x :: xs)
      }
    }
    loop(count, rng, List.empty)
  }
}

import Exercise4._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/Exercise4.scala
  ints(-1)(Simple(1))
  ints(0)(Simple(1))
  ints(1)(Simple(1))
  ints(5)(Simple(1))
*/
