package ch3
import scala.annotation.tailrec
import List._

object Exercise5 {
  @tailrec def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }
}
import Exercise5._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise5.scala
  dropWhile[Int](Nil, _ <= 1)
  dropWhile[Int](List(), _ <= 2)
  dropWhile[Int](List(1), _ <= 5)
  dropWhile[Int](List(1,2,3,4,5), _ <= 3)
  dropWhile[Int](List(1,2,3,4,5), _ <= 4)
  dropWhile[Int](List(1,2,3,4,5), _ <= 5)
*/
