import org.junit.Test
import org.junit.Assert._
import org.scalacheck.Prop

import structures._
import structures.given

class Test1 {
  @Test def t1(): Unit = {
    TotalOrder.laws[Boolean].check()
    TotalOrder.laws[Int].check()
    TotalOrder.laws[String].check()
    TotalOrder.laws(using doubleTotalOrder).check()
    StrictWeakOrder.laws[Int].check()
    StrictPartialOrder.laws(using po2spo(intTotalOrder)).check()
    PartialOrder.laws(using spo2po(intStrictWeakOrder)).check()
    StrictWeakOrder.laws(using tp2swo(intTotalOrder)).check()
    TotalPreorder.laws(using swo2tp(intStrictWeakOrder)).check()
    Preorder.laws[IntDiv].check()
    Preorder.laws[(Int, String)].check()
  }
}