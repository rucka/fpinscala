package ch5
import scala.annotation.tailrec
import Stream._

object Exercise16 {
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

    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = //TODO: to be understand....
      foldRight((z, Stream(z)))((a, p0) => {
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      })._2
  }
}

import Exercise16._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise16.scala
  Stream(1, 2, 3).scanRight(0)(_ + _).toList
*/
