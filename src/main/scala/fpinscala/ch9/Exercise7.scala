object ch9_7 {
  type Parser[A] = A => String
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = ???
  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a,b)))
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => map(p2)(b => f(a, b)))
}
import ch9_7._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise7.scala
*/
