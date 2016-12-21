object ch10_6 {
  import ch10._
  import ch10.Monoid._
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = ???
  def foldRightFromFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(/*f.curried*/ a => b => f(a, b))(z)
  def foldLeftFromFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
}
import ch10_6._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch10/Monoid.scala
  :load src/main/scala/fpinscala/ch10/Exercise6.scala
*/
