object ch10_14 {
  import ch10.Foldable
  import ch10.Monoid
  //import scala.language.higherKinds
  object OptionFoldable extends Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
    override def foldRight[A,B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(a) => f(a, z)
    }
    override def foldLeft[A,B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(a) => f(z, a)
    }
  }
}
import ch10_14._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise14.scala
*/
