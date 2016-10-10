package ch4
import scala.annotation.tailrec
import Option._
import Helpers._

object Exercise4 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      a1 <- a
      b1 <- b
    } yield f(a1, b1)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
}

import Exercise4._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch4/Option.scala
  :load src/main/scala/fpinscala/ch4/Exercise4.scala
*/
