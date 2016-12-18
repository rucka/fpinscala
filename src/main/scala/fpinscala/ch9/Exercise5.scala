object ch9_5 {
  import ch9._
  type Parser[A] = A => String
  def wrap[A](a: => Parser[A]) = succeded(a)
  def manyWithWrap[A](p: Parser[A]): Parser[List[A]] =
    map2(p, wrap(manyWithWrap(p)))(_ :: _) or succeded(List())
}
import ch9_5._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise5.scala
*/
