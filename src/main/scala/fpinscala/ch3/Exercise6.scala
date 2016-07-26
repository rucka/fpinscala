
package ch3
import scala.annotation.tailrec
import List._

object Exercise6 {
  def init[A](l: List[A]): List[A] = {
    @tailrec def loop(l: List[A], acc: scala.collection.immutable.List[A]): List[A] =
      l match {
        case Nil => List(acc :_*)
        case Cons(x, Nil) => List(acc :_*)
        case Cons(x, xs) => loop(xs, acc ++ scala.collection.immutable.List(x) /*(x +: acc.reverse).reverse*/)
      }

    loop(l, scala.collection.immutable.List())
  }
}
import Exercise6._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise6.scala
  init(Nil)
  init(List())
  init(List(1))
  init(List(1,2,3,4,5))
*/
