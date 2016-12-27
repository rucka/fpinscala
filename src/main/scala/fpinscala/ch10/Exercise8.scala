object ch10_8 {
  import ch10.Monoid
  import lib_par.Nonblocking.Par
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.async(b))
    }
  }
}
import ch10_8._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Par.scala
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise8.scala
*/
