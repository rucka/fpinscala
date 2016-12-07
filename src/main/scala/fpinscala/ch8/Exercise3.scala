object ch8_3 {
  trait Prop {
    def check: Boolean
    def &&(p: Prop): Prop = new Prop {
      def check = Prop.this.check && p.check
    }
  }
}
import ch8_3._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch8/Exercise3.scala
*/
