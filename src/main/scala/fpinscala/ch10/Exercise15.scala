object ch10_15 {
  import ch10._
  import scala.language.higherKinds
  def toList[F[_], A](foldable: Foldable[F])(fa: F[A]): List[A] =
    foldable.foldRight(fa)(List[A]())(_ :: _)
}
import ch10_15._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise15.scala
*/
