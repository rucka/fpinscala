object ch10_2 {
  import ch10._
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }
}
import ch10_2._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise2.scala
*/
