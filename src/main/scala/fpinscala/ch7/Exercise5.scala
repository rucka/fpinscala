import Par._
object ch7_5 {
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(Par.unit(List[A]()))((ap, acc) => map2(ap, acc)(_ :: _))
  }
}
import ch7_5._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch7/Par.scala
:load src/main/scala/fpinscala/ch7/Exercise5.scala
*/
