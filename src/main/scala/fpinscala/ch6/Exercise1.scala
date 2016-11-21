package ch6
import scala.annotation.tailrec
import RNG._

object Exercise1 {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
}

import Exercise1._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/Exercise1.scala
  nonNegativeInt(Simple(-1))
  nonNegativeInt(Simple(1))
  nonNegativeInt(Simple(Int.MinValue))
  nonNegativeInt(Simple(Int.MaxValue))
  nonNegativeInt(Simple(-Int.MaxValue))
*/
