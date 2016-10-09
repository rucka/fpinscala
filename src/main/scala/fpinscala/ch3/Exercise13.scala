
package ch3
import scala.annotation.tailrec
import List._

object Exercise13 {
  def reverse[A](ns: List[A]) = foldLeft[A, List[A]](ns, List())((acc, x) => Cons(x, acc))

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((a,b) => f(b, a))
}

import Exercise13._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise13.scala
  foldLeft(List(1,2,3), Nil:List[Int])((b, a) => Cons(a, b))
  foldLeftViaFoldRight(List(1,2,3), Nil:List[Int])((b, a) => Cons(a, b))
  foldRightViaFoldLeft(List(1,2,3), Nil:List[Int])((a, b) => Cons(a, b))

*/
