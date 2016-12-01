object ch7_4 {
  import Par._
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
}
import ch7_4._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch7/Par.scala
:load src/main/scala/fpinscala/ch7/Exercise4.scala
*/
