object ch12 {
  import scala.language.higherKinds
  import lib_monad.Functor

  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)(_(_))

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, mbs) => map2(f(a), mbs)(_ :: _))
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))
    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
      map2(fa, fb)((_,_))
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        def unit[A](a: => A) = (self.unit(a), G.unit(a))
        override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
          (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
      }
    }
  }
}
