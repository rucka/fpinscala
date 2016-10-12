package ch5
import scala.annotation.tailrec
import Stream._

object Exercise3 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {
    def takeWhile(p: A => Boolean): Stream[A] = self match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case Cons(h, _) if !p(h()) => cons(h(), empty)
      case _ => empty
    }

    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }
  }
}

import Exercise3._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise3.scala
*/
