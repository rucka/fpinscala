
package ch3
import scala.annotation.tailrec
import List._

object Exercise15 {
  def append[A](as: List[A], bs: List[A]): List[A] = foldRight(as, bs)(Cons(_, _))
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)
}

import Exercise15._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise15.scala
  concat(List(List(1), List(2, 3), List(4, 5)))
*/
