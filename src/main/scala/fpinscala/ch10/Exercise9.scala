object ch10_9 {
  import ch10.Monoid
  import Monoid._
  def intOrdered = new Monoid[Option[Int]] {
    def op(a1: Option[Int], a2: Option[Int]): Option[Int] = {
      (a1, a2) match {
        case (Some(x), Some(y)) if x <= y => Some(x)
        case _ => zero
      }
    }
    def zero: Option[Int] = None
  }

  def isSorted(v: IndexedSeq[Int]): Boolean =
    foldMapV(v, intOrdered) (Some(_)) != None
}
import ch10_9._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise9.scala
isSorted(IndexedSeq(1,2,3))
isSorted(IndexedSeq(4,2,3))
*/
