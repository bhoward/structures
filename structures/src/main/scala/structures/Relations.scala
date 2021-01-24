package structures

import BoolSyntax._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
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
    def ≲(b: A): Boolean = relates(a, b)
    def ≳(b: A): Boolean = relates(b, a)
    def ≅(b: A): Boolean = (a ≲ b) /\ (b ≲ a)

object Preorder {
  def laws[A : Arbitrary](using Preorder[A]): Prop = all(
    "reflexive" |: forAll { (a: A) => a ≲ a },
    "transitive" |: forAll { (a: A, b: A, c: A) =>
      ((a ≲ b) /\ (b ≲ c)) ==> (a ≲ c)
    }
  )
}

trait Equivalence[A] extends Preorder[A]

object Equivalence {
  def laws[A : Arbitrary](using Equivalence[A]): Prop = all(
    Preorder.laws,
    "symmetric" |: forAll { (a: A, b: A) =>
      (a ≲ b) ==> (b ≲ a)
    }
  )
}

trait PartialOrder[A] extends Preorder[A]:
  extension (a: A)
    def ≤(b: A): Boolean = (a ≲ b)
    def ≥(b: A): Boolean = (a ≳ b)

object PartialOrder {
  def laws[A : Arbitrary](using PartialOrder[A]): Prop = all(
    Preorder.laws,
    "antisymmetric" |: forAll { (a: A, b: A) =>
      (a ≅ b) ==> (a == b)
    }
  )
}

trait TotalPreorder[A] extends Preorder[A]

object TotalPreorder {
  def laws[A : Arbitrary](using TotalPreorder[A]): Prop = all(
    Preorder.laws,
    "total" |: forAll { (a: A, b: A) =>
      (a ≲ b) \/ (b ≲ a)
    }
  )
}

trait TotalOrder[A] extends TotalPreorder[A], PartialOrder[A]

object TotalOrder {
  def laws[A](using TotalOrder[A])(using Arbitrary[A]): Prop = all(
    TotalPreorder.laws,
    PartialOrder.laws
  )
}

trait StrictOrder[A] extends Endorelation[A]:
  extension (a: A)
    def ≨(b: A): Boolean = relates(a, b)
    def ≩(b: A): Boolean = relates(b, a)

object StrictOrder {
  def laws[A : Arbitrary](using StrictOrder[A]): Prop = all(
    "irreflexive" |: forAll { (a: A) => !(a ≨ a) },
    "asymmetric" |: forAll { (a: A, b: A) => !((a ≨ b) /\ (b ≨ a))
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

class IntDiv(val value: Int) extends AnyVal:
  override def toString: String = value.toString

given arbIntDiv: Arbitrary[IntDiv] =
  Arbitrary(for n <- Arbitrary.arbitrary[Int] yield IntDiv(n))

given intDivPreorder: Preorder[IntDiv] with
  override def relates(m: IntDiv, n: IntDiv): Boolean =
    if m.value == 0 then n.value == 0 else (n.value % m.value) == 0
