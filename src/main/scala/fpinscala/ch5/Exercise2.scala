package ch5
import scala.annotation.tailrec
import Stream._

object Exercise2 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {
    def take(n: Int): Stream[A] = self match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    @tailrec def drop(n: Int): Stream[A] = self match {
        case Cons(h, t) if n > 0 => t().drop(n - 1)
        case _ => self
    }

    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }
  }
}

import Exercise2._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise2.scala
  cons(1, cons(2, cons(3, empty))).take(2)
*/
