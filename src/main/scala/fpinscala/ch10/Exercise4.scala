object ch10_4 {
  import ch10._
  trait Prop {
    def &&(b: Prop) : Prop
  }
  trait Gen[A] {
    def flatMap[B](f: A => Gen[B]): Gen[B] = ???
    def map[B](f: A => B): Gen[B] = ???
  }
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = ???

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
  :load src/main/scala/fpinscala/ch10/Monoid.scala
  :load src/main/scala/fpinscala/ch10/Exercise4.scala
*/
