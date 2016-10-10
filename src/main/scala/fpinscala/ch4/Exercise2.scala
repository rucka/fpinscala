package ch4
import scala.annotation.tailrec
import Option._

object Exercise2 {
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m => mean(xs.map(x => math.pow(x - m, 2)))}
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
}

import Exercise2._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch4/Option.scala
  :load src/main/scala/fpinscala/ch4/Exercise2.scala
  mean(Seq(2,4))
  variance(Seq(2,4))
*/
