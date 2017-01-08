object ch11 {
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

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

    def map[A,B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))
    def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List[A]()))((x, y) => map2(x, y)(_ :: _))
    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(List[B]()))((x,y) => map2(f(x), y)(_ :: _))
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))
    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      map(traverse(ms)(a => map(f(a))((_, a))))(_.filter(_._1).map(_._2))
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
      flatMap(f(a))(g)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }
  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }
}
