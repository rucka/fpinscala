object ch9_14 {
  import ch9.{ParseError, Location, Parsers}

  type Parser[+A] = Location => Result[A]
  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
      }
  }
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
    def scope[A](msg: String)(p: Parser[A]): Parser[A] =
      loc => p(loc).mapError(_.push(loc,msg))
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
    implicit def string(s: String): Parser[String] = ??? //Revise the implementation using scope and/or label to provide a meaningful error message in the event of an error.

    def run[A](p: Parser[A])(input: String): Either[ch9.ParseError,A] = ???
  }
}
import ch9_14._
/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch9/Parser.scala
  :load src/main/scala/fpinscala/ch9/Exercise14.scala
*/
