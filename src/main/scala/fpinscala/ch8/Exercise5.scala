object ch8_5 {
  import ch6._
  private val nonNegativeInt = RNG.map(RNG.int)(i => if (i < 0) -(i + 1) else i)
  case class Gen[A](sample: State[RNG, A])
  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.map(nonNegativeInt)(n => start + n % (stopExclusive - start))))
    def unit[A](a: A): Gen[A] = Gen(State.unit(a))
    def boolean: Gen[Boolean] = Gen(State(RNG.map(nonNegativeInt)(_ % 2 == 0)))
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen[List[A]](State.sequence(List.fill(n)(g.sample)))
  }
}
import ch8_5._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/RNG.scala
  :load src/main/scala/fpinscala/ch6/State.scala
  :load src/main/scala/fpinscala/ch8/Exercise5.scala
  Gen.choose(10, 100).sample.run(RNG.Simple(5))
*/
