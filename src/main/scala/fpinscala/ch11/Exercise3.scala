object ch11_3 {
  import scala.language.higherKinds
  import ch11.Monad

  def sequence[F[_],A](monad: Monad[F])(lma: List[F[A]]): F[List[A]] =
    lma.foldRight[F[List[A]]](monad.unit(List[A]()))((x, y) => monad.map2(x, y)(_ :: _))

  def traverse[F[_],A,B](monad: Monad[F])(la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight[F[List[B]]](monad.unit(List[B]()))((x, y) => monad.map2(f(x), y)(_ :: _))
}
import ch11_3._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise3.scala
*/
