object ch9_1 {
  import ch9._
  type Parser[A] = A => String
  def map2FromProduct[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    map(product(p, p2))(f)

  def many1FromMap2[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
}
import ch9_1._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise1.scala
*/
