package ch3

import List._

object Exercise2 {
  def tail[T](l: List[T]) = l match {
    case Nil  => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => xs
  }
}
import Exercise2._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise2.scala
  tail(Nil)
  tail(List())
  tail(List(1))
  tail(List(1,2,3,4,5))
*/
