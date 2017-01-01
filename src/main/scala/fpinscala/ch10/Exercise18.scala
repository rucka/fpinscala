object ch10_18 {
  import ch10.Monoid._
  //“scala> bag(Vector("a", "rose", "is", "a", "rose"))
  //res0: Map[String,Int] = Map(a -> 2, rose -> 2, is -> 1)
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddiction))((a: A) => Map(a -> 1))
}
import ch10_18._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise18.scala
*/
