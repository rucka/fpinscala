object ch12_14 {
  import scala.language.higherKinds
  import lib_monad.Functor
  import ch12.Applicative

  case class Identity[A](a: A)

  val idApplicative = new Applicative[Identity] {
    def unit[A](a: => A) = Identity(a)
    override def map2[A,B,C](fa: Identity[A], fb: Identity[B])(f: (A, B) => C): Identity[C] =
      Identity(f(fa.a, fb.a))
  }
  trait Traverse[F[_]] extends Functor[F] {
    def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_],A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
      traverse(fga)(ga => ga)

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      traverse[Identity,A,B](fa)(a => Identity(f(a)))(idApplicative).a
  }
}
import ch12_14._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Applicative.scala
:load src/main/scala/fpinscala/ch12/Exercise14.scala
*/
