
package ch3
import scala.annotation.tailrec
import List._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/List.scala
  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  //List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
*/
