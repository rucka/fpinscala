package ch5
import scala.annotation.tailrec
import Stream._

object Exercise6 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def headOption: Option[A] = foldRight[Option[A]](None)((x, _) => Some(x))

    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }
  }
}

import Exercise6._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise6.scala
  cons(1, cons(2, cons(3, empty))).headOption
  empty.headOption
*/
