
package ch3
import scala.annotation.tailrec
import List._

object Exercise11 {
  def prod(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def sum(ns: List[Double]) = foldLeft(ns, 0.0)(_ + _)
  def length(ns: List[Double]) = foldLeft(ns, 0.0)((acc, _) => acc + 1)
}

import Exercise11._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise11.scala
  prod(List(1,2,3,4))
  sum(List(1,2,3,4))
  length(List(1,2,3,4))
*/
