object ch12_2 {
  import lib_monad.Functor
  import scala.language.higherKinds

  trait ApplicativeFromUnitApply[F[_]] extends Functor[F] {
    def unit[A](a: A): F[A]
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)
  }

  trait ApplicativeFromUnitMap2[F[_]] extends Functor[F] {
    def unit[A](a: A): F[A]
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)((f, a) => f(a)/*_(_)*/)
  }
}
import ch12_2._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Exercise2.scala
*/
