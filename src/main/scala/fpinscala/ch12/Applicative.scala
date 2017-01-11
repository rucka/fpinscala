object ch12 {
  import scala.language.higherKinds

  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: A): F[A]
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)
  }
}
