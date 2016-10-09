package ch3
import scala.annotation.tailrec
import List._

object Exercise16 {
  def inc(ls: List[Int]): List[Int] = foldRight(ls, Nil:List[Int])((x, acc) => Cons(x + 1, acc))
}

import Exercise16._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise16.scala
  inc(Nil)
  inc(List(1,2,3))
*/
