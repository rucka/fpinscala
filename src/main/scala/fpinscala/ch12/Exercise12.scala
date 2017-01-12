object ch12_12 {
  import scala.language.higherKinds
  import ch12.Applicative
  def sequenceMap[F[_],K,V](A: Applicative[F])(ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(A.unit(Map[K,V]())) { case (facc, (k, fv)) =>
      A.map2(facc, fv)((acc, v) => acc + (k -> v))      
    }
}
import ch12_12._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Applicative.scala
:load src/main/scala/fpinscala/ch12/Exercise12.scala
*/
