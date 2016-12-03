import Par._
object ch7_13 {
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es =>
    map(pa)(a => run(es)(choices(a)).get)(es)
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(r => if (r) t else f)
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices)
}
import ch7_13._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch7/Par.scala
:load src/main/scala/fpinscala/ch7/Exercise13.scala
*/
