import scala.language.higherKinds
import scala.language.implicitConversions
object ch9 {
  trait Parsers[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]
    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
      ParserOps[String] = ParserOps(f(a))

    def map[A, B](a: Parser[A])(f: A => B): Parser[B]
    def map2[A, B, C](a: Parser[A], b: Parser[B])(f: (A, B) => C): Parser[C] =
      map(product(a, b))(f.tupled)

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0) succeded(List())
      else map2(p, listOfN(n -1, p))(_ :: _)

    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) or succeded(List())

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)
    def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]

    def slice[A](p: Parser[A]): Parser[String]

    def succeded[A](a: A): Parser[A] = string("") map (_ => a)

    case class ParserOps[A](p: Parser[A]) {
      def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def many: Parser[List[A]] = self.many(p)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def slice: Parser[String] = self.slice(p)
      def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
      def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    }
  }
}
