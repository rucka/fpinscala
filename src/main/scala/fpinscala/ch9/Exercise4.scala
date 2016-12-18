object ch9_4 {
  import ch9._
  type Parser[A] = A => String
  def listOfNFromMap2[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeded(List())
    else map2(p, listOfNFromMap2(n -1, p))(_ :: _)
}
import ch9_4._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise4.scala
*/
