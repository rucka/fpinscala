package ch4
import scala.{Either => _, None => _, Some => _,_}

sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Helpers {
  implicit class EitherExt[+E, +A](val self: Either[E, A]) extends AnyVal {
    def hello() = s"hello iam a reimplemented ${self.toString}"

    def map[B](f: A => B): Either[E, B] = self match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = self match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
    def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
      self match {
        case Left(_) => b
        case Right(a) => Right(a)
      }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        aa <- self
        bb <- b
      } yield f(aa, bb)
  }
}
import Helpers._
