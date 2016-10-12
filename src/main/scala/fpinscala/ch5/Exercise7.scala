package ch5
import scala.annotation.tailrec
import Stream._

object Exercise7 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def map[B](f: A => B): Stream[B] =
      foldRight[Stream[B]](empty[B])((a,acc) => cons(f(a), acc))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight[Stream[B]](empty)((a, acc) => f(a) append acc)

    def filter(f: A => Boolean): Stream[A] =
      foldRight[Stream[A]](empty)((a, acc) => if (f(a)) cons(a, acc) else acc)

    def append[B>:A](as: => Stream[B]): Stream[B] =
      foldRight(as)((h, t) => cons(h, t))

    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }
  }
}

import Exercise7._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise7.scala
  cons(1, cons(2, cons(3, empty))).map(_ + 1)
  cons(1, cons(2, cons(3, empty))).filter(_ % 2 == 1)
*/
