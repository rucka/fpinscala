object ch12_5 {
  import lib_monad.Monad
  import scala.language.higherKinds

  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E,A] = Right(a)
    def flatMap[A,B](ma: Either[E,A])(f: A => Either[E,B]): Either[E,B] = ma match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }
}
import ch12_5._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Exercise5.scala
*/
