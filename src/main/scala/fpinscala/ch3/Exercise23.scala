package ch3
import scala.annotation.tailrec
import List._

object Exercise23 {
  def zipWith[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
}

import Exercise23._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise23.scala
  zipWith(List(4,5,6), List(5,7,9))(_ + _)
*/
