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
  import Prop._
  case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
    def &&(p: Prop) = Prop {
      (max,n,rng) => run(max,n,rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
    }

    def ||(p: Prop) = Prop {
      (max,n,rng) => run(max,n,rng) match {
        // In case of failure, run the other prop.
        case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
        case x => x
      }
    }

    /* This is rather simplistic - in the event of failure, we simply prepend
     * the given message on a newline in front of the existing message.
     */
    def tag(msg: String) = Prop {
      (max,n,rng) => run(max,n,rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }
  }

  object Prop {
    type SuccessCount = Int
    type TestCases = Int
    type MaxSize = Int
    type FailedCase = String

    sealed trait Result {
      def isFalsified: Boolean
    }
    case object Passed extends Result {
      def isFalsified = false
    }
    case class Falsified(failure: FailedCase,
                         successes: SuccessCount) extends Result {
      def isFalsified = true
    }
    case object Proved extends Result {
      def isFalsified = false
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = ???

    def apply(f: (TestCases,RNG) => Result): Prop =
      Prop { (_,n,rng) => f(n,rng) }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = ???

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = ???

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property.")
      }
  }
}
