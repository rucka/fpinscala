package ch5
import scala.annotation.tailrec
import Stream._

object Exercise1 {
  implicit class StreamExt[+A](val self: Stream[A]) extends AnyVal {
    def toList: List[A] = self match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }

    def toListTailRec: List[A] = {
      @tailrec def loop(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(h, t) => loop(t(), h() :: acc)
      }
      loop(self, List.empty).reverse
    }
  }
}

import Exercise1._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise1.scala
  cons(1, cons(2, cons(3, empty))).toListTailRec
*/
