package ch3
import scala.annotation.tailrec
import List._

object Exercise19 {
  def filter[A](ls: List[A])(f: A => Boolean): List[A] = foldRight(ls, Nil:List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)
  def odd(ls: List[Int]): List[Int] = filter(ls)(_ % 2 == 1)
}

import Exercise19._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise19.scala
  odd(List(1,2,3))
*/
