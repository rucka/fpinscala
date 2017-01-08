object ch11_8 {
  import scala.language.higherKinds
  import ch11.Functor
  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
      compose(map(ma), f)
  }
}
import ch11_8._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise8.scala
*/
