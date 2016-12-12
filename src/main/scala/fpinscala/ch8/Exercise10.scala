object ch8_10 {
  import ch8.Gen
  case class SGen[A](forSize: Int => Gen[A]) //should be case class SGen[+A](forSize: Int => Gen[A]) but ''<console>:79: error: covariant type A occurs in invariant position in type => Int => ch8.Gen[A] of value forSize' occurs
  def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)
}
import ch8_10._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/RNG.scala
  :load src/main/scala/fpinscala/ch6/State.scala
  :load src/main/scala/fpinscala/ch8/Gen.scala
  :load src/main/scala/fpinscala/ch8/Exercise10.scala
*/
