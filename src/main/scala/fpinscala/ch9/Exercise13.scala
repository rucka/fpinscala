object ch9_13 {
  import ch9.{ParseError, Location, Parsers}

  type Parser[+A] = Location => Result[A]
  sealed trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  object Reference extends Parsers[Parser] {
    /** As seen from object Reference, the missing signatures are as follows.
   *  For convenience, these are usable as stub implementations.
   */
    def attempt[A](p: Parser[A]): Parser[A] = ???
    def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = ???
    def label[A](msg: String)(p: Parser[A]): Parser[A] = ???
    def or[A](s1: Parser[A],s2: => Parser[A]): Parser[A] = ???
    def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???
    def slice[A](p: Parser[A]): Parser[String] = loc =>
      p(loc) match {
        case f@Failure(_) => f
        case Success(a, c) => Success(loc.input.slice(loc.offset, c), c)
      }
    def succeded[A](a: A): Parser[A] = l => Success(a, 0)
    implicit def regex(r: scala.util.matching.Regex): Parser[String] = loc => {
      val input = loc.input.slice(0,loc.offset+1)
      r.findPrefixOf(input) match {
        case None => Failure(loc.advanceBy(1).toError("regex " + r))
        case Some(m) => Success(m, m.length)
      }
    }
    implicit def string(s: String): Parser[String] = { loc =>
      val input = loc.input.slice(0,loc.offset+1)
      input.startsWith(s) match {
        case true =>
        Success(s, s.length)
        case _ =>
          Failure(loc.advanceBy(1).toError("Expected " + s))
      }
    }

    def run[A](p: Parser[A])(input: String): Either[ch9.ParseError,A] = ???

  }
}
import ch9_13._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise13.scala
*/
