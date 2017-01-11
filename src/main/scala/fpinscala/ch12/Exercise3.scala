object ch12_3 {
  import lib_monad.Functor
  import scala.language.higherKinds

  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: A): F[A]
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)

    def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])
      (f: (A, B, C) => D): F[D] =
        apply(map2(fa, fb)(f.curried(_)(_)))(fc)

    def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])
      (f: (A, B, C, D) => E): F[E] =
        apply(map3(fa, fb, fc)(f.curried(_)(_)(_)))(fd)
  }
}
import ch12_3._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Exercise3.scala
*/
