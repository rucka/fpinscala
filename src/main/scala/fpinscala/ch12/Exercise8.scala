object ch12_8 {
  import scala.language.higherKinds
  import ch12.Applicative
  def product[F[_],G[_]](F: Applicative[F])(G: Applicative[G]) =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A],G[A]) = (F.unit(a), G.unit(a))
      override def apply[A,B](fab: (F[A => B], G[A => B]))(fa: (F[A],G[A])): (F[B],G[B]) =
        (F.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
}
import ch12_8._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Applicative.scala
:load src/main/scala/fpinscala/ch12/Exercise8.scala
*/
