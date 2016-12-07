object ch8_4 {
  import ch6._
  case class Gen[A](sample: State[RNG, A])
  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      val nonNegativeInt = RNG.map(RNG.int)(i => if (i < 0) -(i + 1) else i)
      val inRange = RNG.map(nonNegativeInt)(n => start + n % (stopExclusive - start))
      Gen(State(inRange))
    }
  }
}
import ch8_4._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/RNG.scala
  :load src/main/scala/fpinscala/ch6/State.scala
  :load src/main/scala/fpinscala/ch8/Exercise4.scala
  Gen.choose(10, 100).sample.run(RNG.Simple(5))
*/
