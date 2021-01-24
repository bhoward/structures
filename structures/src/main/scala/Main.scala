import structures._
import structures.given
import org.scalacheck.Prop.forAll
import org.scalacheck.Test

@main def hello: Unit = {
  println(3 ≤ 4)
  TotalOrder.laws[Int].check(Test.Parameters.default)
  TotalOrder.laws[String].check(Test.Parameters.default)
  TotalOrder.laws(using doubleTotalOrder).check(Test.Parameters.default)
  StrictOrder.laws[Int].check(Test.Parameters.default)
  Preorder.laws[IntDiv].check(Test.Parameters.default)
}
