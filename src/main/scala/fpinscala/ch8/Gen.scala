object ch8 {
  import ch6._
  case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))
    private def listOfN(n: Int): Gen[List[A]] =
      Gen[List[A]](State.sequence(List.fill(n)(sample)))
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size flatMap(listOfN)
  }
  object Gen {
    def unit[A](a: A): Gen[A] = Gen(State.unit(a))
  }
}
