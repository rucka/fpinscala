object ch9_3 {
  import ch9._
  type Parser[A] = A => String
  def manyFromMap2[A](p: Parser[A]): Parser[List[A]] =
    map2(p, manyFromMap2(p))(_ :: _) or succeded(List())
}
import ch9_3._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise3.scala
*/
