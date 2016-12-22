object ch10_4 {
  import ch10._
  import lib_gen._
  import Prop._

  def three[A](x: Gen[A])  = for {
    a <- x
    b <- x
    c <- x
  } yield (a, b, c)

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
:load src/main/scala/fpinscala/lib/Par.Scala
:load src/main/scala/fpinscala/lib/Stream.Scala
:load src/main/scala/fpinscala/lib/State.Scala
:load src/main/scala/fpinscala/lib/Gen.Scala
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise4.scala
*/
