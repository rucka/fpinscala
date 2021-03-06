package ch5
import scala.annotation.tailrec
import Stream._

object Exercise14 {
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
    def take(n: Int): Stream[A] = unfold((self, n)) {
      case (Cons(h, t), i) if i > 1 => Some((h(), (t(), i - 1)))
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case _ => None
    }
    def takeWhile(f: A => Boolean): Stream[A] = unfold(self) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }
    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((self, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }
    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
      unfold((self, s2)) {
        case (Empty, Empty) => None
        case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
        case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      }

    def headOption: Option[A] = self match {
      case Cons(h, _) => Some(h())
      case _ => None
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)


    def startsWith[A](s: Stream[A]): Boolean = zipAll(s)
      .takeWhile(!_._2.isEmpty)
      .forAll {case (h1, h2) => h1 == h2}
  }
}

import Exercise14._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise14.scala
  cons(1, cons(2, cons(3, empty))).startsWith(cons(1, cons(2, empty)))
  cons(1, cons(2, cons(3, empty))).startsWith(cons(1, cons(3, empty)))
*/
