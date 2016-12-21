object ch10_5 {
  import ch10._
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))  
}
import ch10_5._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch10/Monoid.scala
  :load src/main/scala/fpinscala/ch10/Exercise5.scala
*/
