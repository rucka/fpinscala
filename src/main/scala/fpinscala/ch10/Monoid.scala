object ch10 {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }
  object FakeScalaCheck {
    trait Prop {
      def &&(b: Prop) : Prop
    }
    trait Gen[A] {
      def flatMap[B](f: A => Gen[B]): Gen[B] = ???
      def map[B](f: A => B): Gen[B] = ???
    }
    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = ???
    def three[A](x: Gen[A])  = for {
      a <- x
      b <- x
      c <- x
    } yield (a, b, c)
  }
}
