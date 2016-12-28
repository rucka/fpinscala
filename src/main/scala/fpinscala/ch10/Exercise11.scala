object ch10_11 {
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  import ch10.Monoid
  import Monoid._
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(x), Stub(y)) => Stub(x + y)
      case (Stub(x), Part(l, w, r)) => Part(x + l, w, r)
      case (Part(l, w, r), Stub(x)) => Part(l, w, r + x)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
    def zero: WC = Stub("")
  }

  def count(text: String): Int = {
    def unstub(s: String) = s.length min 1
    foldMapV(text.toIndexedSeq, wcMonoid) { c =>
      if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    } match {
      case Stub(x) => unstub(x)
      case Part(l, c, r) => c + unstub(l) + unstub(r)
    }
  }
}
import ch10_11._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch10/Monoid.scala
:load src/main/scala/fpinscala/ch10/Exercise11.scala
*/
