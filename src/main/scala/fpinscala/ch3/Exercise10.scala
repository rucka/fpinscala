
package ch3
import scala.annotation.tailrec
import List._

object Exercise10 {
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) =>
          foldLeft(xs, f(z, x))(f)
      }

    def p(ns: List[Double]) = foldLeft(ns, 1.0)((x, y) => {
        println(s"p($x, $y)")
        x * y
    })
}

import Exercise10._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise10.scala
  p(List(1,2,3,4))

*/
