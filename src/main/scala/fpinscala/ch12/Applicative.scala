object ch12 {
  import scala.language.higherKinds

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
    def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] =
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }
  }
  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: A): F[A]
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)
  }
}
