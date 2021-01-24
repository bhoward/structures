import structures._
import structures.given
import org.scalacheck.Prop.forAll
import org.scalacheck.Test

@main def hello: Unit = {
  val foo = forAll { (l1: List[Int], l2: List[Int]) => l1.size + l2.size == (l1 ::: l2).size }
  val result = Test.check(Test.Parameters.default, foo)
  println(result.status)
  println(3 ≤ 4)
}
