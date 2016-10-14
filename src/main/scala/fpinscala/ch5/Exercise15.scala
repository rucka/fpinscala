package ch5
import scala.annotation.tailrec
import Stream._

object Exercise15 {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((h, s)) => cons(h, unfold(s)(f))
    }

  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {
    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }

    def map[B](f: A => B): Stream[B] = unfold(self) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
    def append[B>:A](as: => Stream[B]): Stream[B] =
      foldRight(as)((h, t) => cons(h, t))

    def tails: Stream[Stream[A]] = unfold(self) {
      case s@Cons(h1, t1) => Some((s, t1()))
      case _ => None
    } append Stream(empty)
  }
}

import Exercise15._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise15.scala
  cons(1, cons(2, cons(3, empty))).startsWith(cons(1, cons(2, empty)))
  cons(1, cons(2, cons(3, empty))).startsWith(cons(1, cons(3, empty)))
*/
