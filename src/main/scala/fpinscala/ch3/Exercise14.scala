
package ch3
import scala.annotation.tailrec
import List._

object Exercise14 {
  private def reverse[A](ns: List[A]) = foldLeft[A, List[A]](ns, List())((acc, x) => Cons(x, acc))
  def appendr[A](as: List[A], item: A): List[A] = foldRight(as, List(item))(Cons(_, _))
  def appendl[A](as: List[A], item: A): List[A] = foldLeft(reverse(as), List(item))((acc, x) => Cons(x, acc))
}

import Exercise14._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise14.scala
  appendl(Nil, 1)
  appendl(List(1), 2)
  appendl(List(1, 2), 3)
  appendr(Nil, 1)
  appendr(List(1), 2)
  appendr(List(1, 2), 3)
*/
