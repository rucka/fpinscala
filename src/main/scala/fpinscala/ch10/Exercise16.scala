object ch10_16 {
  import ch10.Monoid
  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    def op(x: (A, B), y: (A, B)): (A, B) = (a.op(x._1, y._1), b.op(x._2, y._2))
    def zero: (A, B) = (a.zero, b.zero)
  }
}
import ch10_16._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise16.scala
*/
