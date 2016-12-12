object ch8_9 {
  import ch8.Gen
  import Prop._
  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop { (t, rnd) =>
      run(t, rnd) match {
          case Passed => p.run(t, rnd)
          case x => x
        }
      }

    def ||(p: Prop): Prop = Prop { (t, rnd) =>
      run(t, rnd) match {
          case Falsified(msg, _) => p.tag(msg).run(t, rnd)
          case x => x
        }
      }
    def tag(message: String): Prop = Prop { (t, rnd) =>
      run(t, rnd) match {
        case Falsified(msg, s) => Falsified(s"$msg\n$message", s)
        case x => x
      }
    }
  }
  object Prop {
    sealed trait Result {
      def isFalsified: Boolean
    }
    case object Passed extends Result {
      def isFalsified = false
    }
    type TestCases = Int
    type SuccessCount = Int
    type FailedCase = String
    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      def isFalsified = true
    }
  }
}
import ch8_9._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch6/RNG.scala
  :load src/main/scala/fpinscala/ch6/State.scala
  :load src/main/scala/fpinscala/ch8/Gen.scala
  :load src/main/scala/fpinscala/ch8/Exercise9.scala
*/
