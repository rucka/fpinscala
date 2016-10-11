package ch4
import scala.annotation.tailrec
import Option._
import Helpers._

object Exercise5 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      a1 <- a
      b1 <- b
    } yield f(a1, b1)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))
  def identity[A](a: A): A = a
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}

import Exercise5._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch4/Option.scala
  :load src/main/scala/fpinscala/ch4/Exercise5.scala
*/
