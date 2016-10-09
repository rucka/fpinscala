package ch3
import scala.annotation.tailrec
import List._

object Exercise18 {
  def map[A,B](ls: List[A])(f: A => B): List[B] = foldRight(ls, Nil:List[B])((x, acc) => Cons(f(x), acc))
}

import Exercise18._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise18.scala
  map(List(1,2,3))(_.toString)
*/
