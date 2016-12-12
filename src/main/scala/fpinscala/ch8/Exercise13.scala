object ch8_13 {
  import ch8.Prop._
  def listOf1[A](g: ch8.Gen[A]): ch8.SGen[List[A]] = ch8.SGen(n => g.listOfN(n max 1))
  val smallInt = ch8.Gen.choose(-10, 10)
  val maxProp1 = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }
}
import ch8_13._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/RNG.scala
  :load src/main/scala/fpinscala/ch6/State.scala
  :load src/main/scala/fpinscala/ch8/Gen.scala
  :load src/main/scala/fpinscala/ch8/Exercise13.scala
*/
