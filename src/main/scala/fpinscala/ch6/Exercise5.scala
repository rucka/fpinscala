package ch6
import scala.annotation.tailrec
import RNG._

object Exercise5 {
  val int: Rand[Int] = _.nextInt
  val nonNegativeInt = map(int)(i => if (i < 0) -(i + 1) else i)
  def double: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
}

import Exercise5._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/Exercise5.scala
  double(Simple(1))
*/
