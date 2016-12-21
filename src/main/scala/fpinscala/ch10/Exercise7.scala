object ch10_7 {
  import ch10._
  import ch10.Monoid._
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) m.zero
    if (v.length == 1) f(v.head)
    val pairs = v.splitAt(v.length / 2)
    m.op(foldMapV(pairs._1, m)(f), foldMapV(pairs._1, m)(f))
  }
}
import ch10_7._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch10/Monoid.scala
  :load src/main/scala/fpinscala/ch10/Exercise7.scala
*/
