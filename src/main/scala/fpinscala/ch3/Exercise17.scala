package ch3
import scala.annotation.tailrec
import List._

object Exercise17 {
  def toStrings(ls: List[Double]): List[String] = foldRight(ls, Nil:List[String])((x, acc) => Cons(x.toString, acc))
}

import Exercise17._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise17.scala
  toStrings(Nil)
  toStrings(List(1,2,3))
*/
