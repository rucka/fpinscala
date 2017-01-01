object ch10_17 {
  import ch10.Monoid
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def zero: A => B = _ => B.zero
    def op (f: A => B, g: A => B) : A => B = a => B.op(f(a), g(a))
  }
}
import ch10_17._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise17.scala
*/
