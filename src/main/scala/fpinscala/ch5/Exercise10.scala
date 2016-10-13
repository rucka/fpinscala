package ch5
import scala.annotation.tailrec
import Stream._

object Exercise10 {
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
  def fibs: Stream[Int] = {
    def loop(a1: Int, a2: Int): Stream[Int] = cons(a1, loop(a2, a1 + a2))
    loop(0, 1)
  }
}

import Exercise10._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise10.scala
  fibs.take(10).toList
*/
