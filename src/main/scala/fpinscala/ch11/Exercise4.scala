object ch11_4 {
  import scala.language.higherKinds
  import ch11.Monad

  def replicateM[F[_],A](monad: Monad[F])(n: Int, ma: F[A]): F[List[A]] =
    monad.sequence((1 to n).toList.map(_ => ma))
}
import ch11_4._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise4.scala
*/
