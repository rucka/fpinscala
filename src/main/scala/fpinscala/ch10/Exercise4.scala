object ch10_4 {
  import ch10._
  import ch10.FakeScalaCheck._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associativity: Prop = forAll(three(gen))(x =>
      m.op(x._1, m.op(x._2, x._3)) == m.op(m.op(x._1, x._2), x._3)
    )
    val identity: Prop = forAll(gen)(x => m.op(x, m.zero) == m.zero)
    associativity && identity
  }
}
import ch10_4._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch10/Monoid.scala
  :load src/main/scala/fpinscala/ch10/Exercise4.scala
*/
