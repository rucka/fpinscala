object ch9_8 {
  type Parser[A] = A => String
  def succeded[A](a: A): Parser[A] = ???
  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeded(f(a)))
}
import ch9_8._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise8.scala
*/
