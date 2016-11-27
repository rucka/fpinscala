object ch7_1 {
  trait Par[A]
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
}
import ch7_1._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch7/Exercise1.scala
*/
