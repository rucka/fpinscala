package ch4
import scala.{Option => _, None => _, Some => _,_}

sealed trait Option[+A]

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Helpers {
  implicit class OptionExt[+A](val self: Option[A]) extends AnyVal {
    def hello() = s"hello iam a reimplemented ${self.toString}"

    def map[B](f: A => B): Option[B] = self match {
      case None => None
      case Some(v) => Some(f(v))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    def getOrElse[B >: A](default: => B): B = self match {
      case None => default
      case Some(v) => v
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
    def filter(f: A => Boolean): Option[A] = this flatMap(v => if (f(v)) Some(v) else None)
  }
}
import Helpers._
