object ch11_7 {
  import scala.language.higherKinds
  import ch11.Monad
  def compose[F[_],A,B,C](monad: Monad[F])(f: A => F[B], g: B => F[C]): A => F[C] = a =>
    monad.flatMap(f(a))(g)
}
import ch11_7._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise7.scala
*/
