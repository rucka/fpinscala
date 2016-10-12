package ch5
import scala.annotation.tailrec
import Stream._

object Exercise5 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }
  }
}

import Exercise5._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise5.scala
  cons(1, cons(2, cons(3, empty))).takeWhile(_ < 2)
  cons(1, cons(2, cons(3, empty))).takeWhile(_ > 10)
*/
