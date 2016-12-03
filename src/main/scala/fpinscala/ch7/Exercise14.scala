import Par._
object ch7_14 {
  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }
  def identity[A](a: A) = a
  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())
  def joinViaFlatMap[A](p: Par[Par[A]]): Par[A] = flatMap(p)(identity)
  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = join(map(p)(f))
}
import ch7_14._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch7/Par.scala
:load src/main/scala/fpinscala/ch7/Exercise14.scala
*/
