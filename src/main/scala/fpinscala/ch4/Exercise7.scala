package ch4
import scala.annotation.tailrec
import Either._

object Exercise7 {
  def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldRight[Either[E, List[B]]](Right(Nil))((a,acc) => map2(f(a), acc)(_ :: _))

  def identity[A](a: A): A = a
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

}

import Exercise7._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch4/Either.scala
  :load src/main/scala/fpinscala/ch4/Exercise7.scala
*/
