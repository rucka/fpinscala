
package ch3
import scala.annotation.tailrec
import List._

object Exercise12 {
  def reverse(ns: List[Double]) = foldLeft[Double, List[Double]](ns, List())((acc, x) => Cons(x, acc))
}

import Exercise12._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise12.scala
  reverse(Nil)
  reverse(List())
  reverse(List(1,2,3,4))
*/
