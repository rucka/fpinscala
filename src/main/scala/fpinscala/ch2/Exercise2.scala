import scala.annotation.tailrec

object Exercise2 {
    @tailrec def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = as match {
        case Array() | Array(_) => true
        case Array(x, y, _*) if !ordered(x,y) => false
        case Array(x, y, _*)  => isSorted(as.tail, ordered)
    }
}

import Exercise2._

/*
from repl you can test typing: 
    :load src/main/scala/fpinscala/ch2/Exercise2.scala
    isSorted[Int](Array(1,2,3,4), (a,b) => {a <= b})
    isSorted[Int](Array('a','b','c','d'), (a,b) => {a <= b})
    isSorted[Int](Array(1,3,2,4), (a,b) => {a <= b})
    isSorted[Int](Array(4,3,2,4), (a,b) => {a <= b})
    isSorted[Int](Array('a','c','b','d'), (a,b) => {a <= b})
*/
