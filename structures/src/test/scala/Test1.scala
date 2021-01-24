import org.junit.Test
import org.junit.Assert._
import org.scalacheck.Prop

import structures._
import structures.given

class Test1 {
  @Test def t1(): Unit = {
    TotalOrder.laws[Int].check()
    TotalOrder.laws[String].check()
    TotalOrder.laws(using doubleTotalOrder).check()
    StrictOrder.laws[Int].check()
    Preorder.laws[IntDiv].check()
  }
}