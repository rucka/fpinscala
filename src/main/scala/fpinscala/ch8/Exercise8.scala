object ch8_8 {
  import ch8.Gen
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    Gen.choose(0, (g1._2 * 100 + g2._2 * 100).toInt).flatMap(n => if (n < g1._2) g1._1 else g2._1)
}
import ch8_8._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/RNG.scala
  :load src/main/scala/fpinscala/ch6/State.scala
  :load src/main/scala/fpinscala/ch8/Gen.scala
  :load src/main/scala/fpinscala/ch8/Exercise8.scala
*/
