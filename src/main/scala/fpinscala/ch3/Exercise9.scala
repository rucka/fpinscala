
package ch3
import scala.annotation.tailrec
import List._

object Exercise9 {
    def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)
}

import Exercise9._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  :load src/main/scala/fpinscala/ch3/Exercise9.scala
  length(List())
  length(Nil)
  length(List(1))
  length(List(1,2,3))
*/
