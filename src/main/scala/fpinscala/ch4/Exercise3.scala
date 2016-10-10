package ch4
import scala.annotation.tailrec
import Option._
import Helpers._

object Exercise3 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      a1 <- a
      b1 <- b
    } yield f(a1, b1)
}

import Exercise3._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch4/Option.scala
  :load src/main/scala/fpinscala/ch4/Exercise3.scala
*/
