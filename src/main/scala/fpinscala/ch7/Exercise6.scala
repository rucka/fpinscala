import Par._
object ch7_6 {
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val hasFilter: Par[List[Boolean]] = parMap[A, Boolean](as)(f)
    map(hasFilter)(res => (res zip as).filter(_._1).map(_._2))
  }
}
import ch7_6._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch7/Par.scala
:load src/main/scala/fpinscala/ch7/Exercise6.scala
*/
