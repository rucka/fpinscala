package ch5
import scala.annotation.tailrec
import Stream._

object Exercise4 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {
    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }
  }
}

import Exercise4._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise4.scala
  cons(1, cons(2, cons(3, empty))).forAll(_ < 2)
  cons(1, cons(2, cons(3, empty))).forAll(_ > 10)
*/
