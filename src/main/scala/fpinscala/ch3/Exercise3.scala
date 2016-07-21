package ch3

import List._

object Exercise3 {
  def setHead[T](l: List[T], newValue: T) = l match {
    case Nil  => Nil
    case Cons(x, Nil) => Cons(newValue, Nil)
    case Cons(x, xs) => Cons(newValue, xs)
  }
}
import Exercise3._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise3.scala
  setHead(Nil, 5)
  setHead(List(), 5)
  setHead(List(1), 5)
  setHead(List(1,2,3,4,5), 5)
*/
