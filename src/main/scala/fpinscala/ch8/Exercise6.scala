object ch8_6 {
  def flatMap[A, B](g: ch8.Gen[A])(f: A => ch8.Gen[B]): ch8.Gen[B] =
    ch8.Gen(g.sample.flatMap(a => f(a).sample))
  def listOfN[A](n: Int, g: ch8.Gen[A]): ch8.Gen[List[A]] =
    ch8.Gen[List[A]](State.sequence(List.fill(n)(g.sample)))
  def listOfN[A](g: ch8.Gen[A])(size: ch8.Gen[Int]): ch8.Gen[List[A]] =
    flatMap(size)(n => listOfN(n, g))
}
import ch8_6._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/RNG.scala
  :load src/main/scala/fpinscala/ch6/State.scala
  :load src/main/scala/fpinscala/ch8/Gen.scala
  :load src/main/scala/fpinscala/ch8/Exercise6.scala
*/
