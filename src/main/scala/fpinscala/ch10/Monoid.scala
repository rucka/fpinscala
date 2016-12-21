object ch10 {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }
  object Monoid {
    val intAddition = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2
      def zero: Int = 0
    }
    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 * a2
      def zero: Int = 1
    }
    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
      def zero: Boolean = false
    }
    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
      def zero: Boolean = true
    }
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
      def zero: Option[A] = None
    }
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      def op(f: A => A, g: A => A): A => A = f compose g
      def zero: A => A = a => a
    }
    def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
      def op(x: A, y: A): A = m.op(y, x)
      val zero = m.zero
    }
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
