package ch5
import scala.annotation.tailrec
import Stream._

object Exercise11 {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((h, s)) => cons(h, unfold(s)(f))
    }
}

import Exercise11._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch5/Stream.scala
  :load src/main/scala/fpinscala/ch5/Exercise11.scala
*/
