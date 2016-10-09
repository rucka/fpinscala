package ch3
import scala.annotation.tailrec

object Exercise24 {
  @annotation.tailrec def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (h1 :: t1, h2 :: t2) if h1 == h2 => hasSubsequence(t1, t2)
    case (h1 :: t1, _) if h1 == sub.head => hasSubsequence(t1, sub.tail)
    case (h1 :: t1, _) => hasSubsequence(t1, sub)
  }
}

import Exercise24._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/Exercise24.scala
  hasSubsequence(List(1,2,3,4), List(1,2))
  hasSubsequence(List(1,2,3,4), List(2,3))
  hasSubsequence(List(1,2,3,4), List(2,3,5))
  hasSubsequence(List(1,2,3,4), List(2,4))
*/
