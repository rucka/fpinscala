import Par._
object ch7_11 {
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(c => if (c) 0 else 1))(List(t, f))
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val index = run(es)(n).get
    choices(index)(es)
  }
}
import ch7_11._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch7/Par.scala
:load src/main/scala/fpinscala/ch7/Exercise11.scala
*/
