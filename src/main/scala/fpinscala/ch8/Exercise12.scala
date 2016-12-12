object ch8_12 {
  def listOf[A](g: ch8.Gen[A]): ch8.SGen[List[A]] = ch8.SGen(n => g.listOfN(n))
}
import ch8_12._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/RNG.scala
  :load src/main/scala/fpinscala/ch6/State.scala
  :load src/main/scala/fpinscala/ch8/Gen.scala
  :load src/main/scala/fpinscala/ch8/Exercise12.scala
*/
