package ch5
import scala.annotation.tailrec
import Stream._

object Exercise9 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {
    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = self match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }
  def from(a: Int): Stream[Int] = cons(a, from(a + 1))
}

import Exercise9._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise9.scala
  from(5).take(3)
*/
