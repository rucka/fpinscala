package ch3
import scala.annotation.tailrec
import List._

object Exercise20 {
  def flatMap[A, B](as: List[A])(f: A => List[B]) : List[B] =
    foldRight(as, Nil:List[B])((x, acc) => append(f(x), acc))
}

import Exercise20._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise20.scala
  flatMap(List(1,2,3))(i => List(i,i))
*/
