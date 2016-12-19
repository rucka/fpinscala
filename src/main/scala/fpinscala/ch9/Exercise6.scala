object ch9_6 {
  import scala.language.implicitConversions
  import scala.util.matching.Regex
  type Parser[A] = A => String
  implicit def regex(r: Regex): Parser[String] = ???
  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???
  def char(c: Char): Parser[Char] = ???
  def parse = {
    val regp = "[0-9]".r
    flatMap(regp)(s => listOfN(s.toInt,char('a')))
  }
}
import ch9_6._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise6.scala
*/
