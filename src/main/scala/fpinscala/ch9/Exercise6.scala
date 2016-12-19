object ch9_6 {
  implicit def regex(r: Regex): Parser[String] = ???
  def parse: Parser[String] = {
    regex("[0-9]".r).flatMap(s => listOfN(s.toInt, char('a'))))
  }
}
import ch9_6._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise6.scala
*/
