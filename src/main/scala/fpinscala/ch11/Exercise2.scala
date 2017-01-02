object ch11_2 {
  import lib_state.State
  import ch11.Monad
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A) = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]) =
      st flatMap f
  }
}
import ch11_2._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/State.scala
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise2.scala
*/
