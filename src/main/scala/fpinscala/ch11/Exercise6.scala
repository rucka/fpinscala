object ch11_6 {
  import scala.language.higherKinds
  import ch11.Monad

  def filterM[F[_],A](monad: Monad[F])(ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val lma: F[List[(Boolean, A)]] = monad
      .traverse(ms)(a => monad.map(f(a))((_, a)))
    monad
      .map(lma)(_.filter(_._1).map(_._2))
  }

  def _filterM[F[_],A](monad: Monad[F])(ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => monad.unit(List[A]())
      case h :: t => monad.flatMap(f(h))(b =>
        if (!b) _filterM(monad)(t)(f)
        else monad.map(_filterM(monad)(t)(f))(h :: _)
      )
    }
}
import ch11_6._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise6.scala
*/
