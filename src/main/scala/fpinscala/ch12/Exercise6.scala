object ch12_6 {
  import ch12.Applicative

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing] {
      def merge[E](failure: Failure[E]): Failure[E] = ???/*{
        Failure(head, tail ++ Vector(failure.head) ++ failure.tail)
      }*/
    }

  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    def unit[A](a: => A) = Success(a)

    override def apply[A,B](fab: Validation[E,A => B])(fa: Validation[E,A]) =
      (fab, fa) match {
        case (f1@Failure(_,_), f2@Failure(_,_)) => ???
        case (f1@Failure(_,_), _) => f1
        case (_, f2@Failure(_,_)) => f2
        case (Success(f), Success(a)) => Success(f(a))
      }
  }
}
import ch12_6._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Applicative.scala
:load src/main/scala/fpinscala/ch12/Exercise6.scala
*/
