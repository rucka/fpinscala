object ch11_13 {
  import scala.language.higherKinds
  trait Monad[F[_]] {
    def map[A,B](ma: F[A])(f: A => B): F[B]
    def unit[A](a: => A): F[A]
    def join[A](mma: F[F[A]]): F[A]
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
      join(map(f(a))(g))
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
      join(map(ma)(f))
  }
}
import ch11_13._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Exercise13.scala
*/
