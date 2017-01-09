object ch11_17 {
  import scala.language.higherKinds
  import ch11.Monad

  case class Id[A](value: A)
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A,B](ma: Id[A])(f: A => Id[B]) = f(ma.value)
  }
}
import ch11_17._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise17.scala
*/
