package ch3
import scala.annotation.tailrec
import List._

object Exercise4 {
  @tailrec def drop[T](l: List[T], n: Int): List[T] = l match {
    case _ if n <= 0 => l
    case Nil  => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) if n == 1 => xs
    case Cons(x, xs) => drop(xs, n - 1)
  }
}
import Exercise4._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise4.scala
  drop(Nil, 1)
  drop(List(), 2)
  drop(List(1), 5)
  drop(List(1,2,3,4,5), 3)
  drop(List(1,2,3,4,5), 4)
  drop(List(1,2,3,4,5), 5)
*/
