package ch6
import scala.annotation.tailrec
import RNG._

object Exercise2 {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
}

import Exercise2._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/Exercise2.scala
  double(Simple(-1))
  double(Simple(1))
  double(Simple(Int.MinValue))
  double(Simple(Int.MaxValue))
  double(Simple(-Int.MaxValue))
*/
