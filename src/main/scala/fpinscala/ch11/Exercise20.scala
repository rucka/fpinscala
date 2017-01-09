object ch11_20 {
  import ch11.Monad

  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
      def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
      def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
        Reader(r => f(st.run(r)).run(r))
    }
  }
}
import ch11_20._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise20.scala
*/
