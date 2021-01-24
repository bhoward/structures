package structures

import BoolSyntax._
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{forAll, propBoolean, all}

trait Predicate[A]:
  def holdsAt(a: A): Boolean

trait Relation[A, B] extends Predicate[(A, B)]:
  def relates(a: A, b: B): Boolean
  override def holdsAt(p: (A, B)) = p match
    case (a, b) => relates(a, b)

trait Endorelation[A] extends Relation[A, A]

trait Preorder[A] extends Endorelation[A]:
  extension (a: A)
    def pre_<=(b: A): Boolean = relates(a, b)
    def pre_>=(b: A): Boolean = (b pre_<= a)
    def pre_=~(b: A): Boolean = (a pre_<= b) /\ (b pre_<= a)

object Preorder {
  def laws[A](using r: Preorder[A])(using Arbitrary[A]): Prop = all(
    "reflexive" |: forAll { (a: A) => a pre_<= a },
    "transitive" |: forAll { (a: A, b: A, c: A) =>
      ((a pre_<= b) /\ (b pre_<= c)) ==> (a pre_<= c)
    }
  )
}

trait Equivalence[A] extends Preorder[A]:
  extension (a: A)
    def eq_==(b: A): Boolean = relates(a, b)

object Equivalence {
  def laws[A](using Equivalence[A])(using Arbitrary[A]): Prop = all(
    Preorder.laws,
    "symmetric" |: forAll { (a: A, b: A) =>
      (a eq_== b) ==> (b eq_== a)
    }
  )
}

trait PartialOrder[A] extends Preorder[A]:
  extension (a: A)
    def po_<=(b: A): Boolean = (a pre_<= b)
    def po_>=(b: A): Boolean = (a pre_>= b)
    def po_=~(b: A): Boolean = (a pre_=~ b)

object PartialOrder {
  def laws[A](using PartialOrder[A])(using Arbitrary[A]): Prop = all(
    Preorder.laws,
    "antisymmetric" |: forAll { (a: A, b: A) =>
      (a po_=~ b) ==> (a == b)
    }
  )
}

trait TotalPreorder[A] extends Preorder[A]:
  extension (a: A)
    def tp_<=(b: A): Boolean = (a pre_<= b)
    def tp_>=(b: A): Boolean = (a pre_>= b)
    def tp_=~(b: A): Boolean = (a pre_=~ b)

object TotalPreorder {
  def laws[A](using TotalPreorder[A])(using Arbitrary[A]): Prop = all(
    Preorder.laws,
    "total" |: forAll { (a: A, b: A) =>
      (a tp_<= b) \/ (b tp_<= a)
    }
  )
}

trait TotalOrder[A] extends TotalPreorder[A], PartialOrder[A]:
  extension (a: A)
    def to_<=(b: A): Boolean = (a tp_<= b)
    def to_>=(b: A): Boolean = (a tp_>= b)
    def to_=~(b: A): Boolean = (a tp_=~ b)

object TotalOrder {
  def laws[A](using TotalOrder[A])(using Arbitrary[A]): Prop = all(
    TotalPreorder.laws,
    PartialOrder.laws
  )
}

trait StrictOrder[A] extends Endorelation[A]:
  extension (a: A)
    def so_<(b: A): Boolean = relates(a, b)
    def so_>(b: A): Boolean = (b so_< a)

object StrictOrder {
  def laws[A](using StrictOrder[A])(using Arbitrary[A]): Prop = all(
    "irreflexive" |: forAll { (a: A) => !(a so_< a) },
    "asymmetric" |: forAll { (a: A, b: A) => !((a so_< b) /\ (b so_< a))
    }
  )
}

// PER, functions, partial functions, etc.?
// define more instances for common examples and constructions

given intTotalOrder: TotalOrder[Int] with
  override def relates(m: Int, n: Int): Boolean = m <= n

given intStrictOrder: StrictOrder[Int] with
  override def relates(m: Int, n: Int): Boolean = m < n

given stringTotalOrder: TotalOrder[String] with
  override def relates(s: String, t: String): Boolean = s <= t

object doubleTotalOrder extends TotalOrder[Double] {
  override def relates(x: Double, y: Double): Boolean = x <= y
}

case class IntDiv(val value: Int) extends AnyVal

object IntDiv {
  given Arbitrary[IntDiv] =
    Arbitrary(for n <- Arbitrary.arbitrary[Int] yield IntDiv(n))

  given Preorder[IntDiv] with
    override def relates(m: IntDiv, n: IntDiv): Boolean =
      if m.value == 0 then n.value == 0 else (n.value % m.value) == 0
}