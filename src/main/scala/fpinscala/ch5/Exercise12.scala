package ch5
import scala.annotation.tailrec
import Stream._

object Exercise12 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {
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
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((h, s)) => cons(h, unfold(s)(f))
    }

  val ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))
  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))
  def from(a: Int): Stream[Int] = unfold(a)(s => Some(s, s + 1))
  val fibs: Stream[Int] = unfold((0, 1)){case (f0, f1) => Some(f1, (f1, f0 + f1))}
}

import Exercise12._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise12.scala
  ones.take(10).toList
  constant(2).take(10).toList
  from(2).take(10).toList
  fibs.take(10).toList
*/
