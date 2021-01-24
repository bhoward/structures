import structures._
import structures.given
import org.scalacheck.Prop.forAll
import org.scalacheck.Test

@main def hello: Unit = {
  println(3 ≤ 4)
  val foo = TotalOrder.laws[String].check(Test.Parameters.default)
  println(foo)
  val bar = TotalOrder.laws(using Foo.doubleTotalOrder).check(Test.Parameters.default)
  println(bar)
}
