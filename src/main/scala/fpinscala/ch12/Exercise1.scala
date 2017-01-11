object ch12_1 {
  import lib_monad.Functor
  import scala.language.higherKinds

  trait Applicative[F[_]] extends Functor[F] {
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: A): F[A]

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, mbs) => map2(f(a), mbs)(_ :: _))
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))
    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
      map2(fa, fb)((_,_))
  }
}
import ch12_1._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Exercise1.scala
*/
