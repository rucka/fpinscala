object ch8 {
  import ch6._
  case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))
    def listOfN(n: Int): Gen[List[A]] =
      Gen[List[A]](State.sequence(List.fill(n)(sample)))
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap(listOfN)
    def unsized = SGen(_ => this)
  }
  case class SGen[A](forSize: Int => Gen[A])
  object Gen {
    private val nonNegativeInt = RNG.map(RNG.int)(i => if (i < 0) -(i + 1) else i)
    def unit[A](a: A): Gen[A] = Gen(State.unit(a))
    def boolean: Gen[Boolean] = Gen(State(RNG.map(nonNegativeInt)(_ % 2 == 0)))
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.map(nonNegativeInt)(n => start + n % (stopExclusive - start))))
  }
}
