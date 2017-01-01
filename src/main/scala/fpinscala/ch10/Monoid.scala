object ch10 {
  import scala.language.higherKinds
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }
  trait Foldable[F[_]] {
    import Monoid._
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
    def toList[A](fa: F[A]): List[A] =
      foldRight(fa)(List[A]())(_ :: _)
  }
  object Monoid {
    val intAddiction = new Monoid[Int] {
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
    def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
                              b.getOrElse(k, V.zero)))
        }
      }
    def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
      new Monoid[(A, B)] {
        def op(x: (A, B), y: (A, B)) =
          (A.op(x._1, y._1), B.op(x._2, y._2))
        val zero = (A.zero, B.zero)
      }
    def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
      new Monoid[A => B] {
        def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))
        val zero: A => B = a => B.zero
      }
    def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (v.length == 0) m.zero
      else if (v.length == 1) f(v.head)
      else {
        val pairs = v.splitAt(v.length / 2)
        m.op(foldMapV(pairs._1, m)(f), foldMapV(pairs._2, m)(f))
      }
    }
  }
}
