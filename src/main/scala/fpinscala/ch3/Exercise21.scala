package ch3
import scala.annotation.tailrec
import List._

object Exercise21 {
  def filter[A](ls: List[A])(f: A => Boolean): List[A] =
    flatMap(ls)(x => if (f(x)) List(x) else Nil)
}

import Exercise21._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise21.scala
  filter(List(1,2,3))(_ % 2 == 1)
*/
