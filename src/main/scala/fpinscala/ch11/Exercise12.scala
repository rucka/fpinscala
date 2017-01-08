object ch11_12 {
  import scala.language.higherKinds
  import ch11.Monad

  def join[F[_],A](monad: Monad[F])(mma: F[F[A]]): F[A] =
    monad.flatMap(mma)(identity)
}
import ch11_12._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise12.scala
*/
