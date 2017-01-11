object ch12_9 {
  import scala.language.higherKinds
  import ch12.Applicative
  def compose[F[_],G[_]](F: Applicative[F])(G: Applicative[G]) =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        F.map2(fga, fgb)(G.map2(_,_)(f))
    }

}
import ch12_9._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Applicative.scala
:load src/main/scala/fpinscala/ch12/Exercise9.scala
*/
