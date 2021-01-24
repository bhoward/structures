package structures

import Props._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll, propBoolean, all}

trait Predicate[A]:
  def holdsAt(a: A): Boolean

trait Relation[A, B] extends Predicate[(A, B)]:
  def relates(a: A, b: B): Boolean
  override def holdsAt(p: (A, B)) = p match
    case (a, b) => relates(a, b)

trait Endorelation[A] extends Relation[A, A]

trait Preorder[A : Arbitrary] extends Endorelation[A]:
  extension (a: A)
    def ≲(b: A): Boolean = relates(a, b)
    def ≳(b: A): Boolean = relates(b, a)
    def ≅(b: A): Boolean = (a ≲ b) /\ (b ≲ a)

  val preorderProps = all(
    "reflexive" |: forAll { (a: A) => a ≲ a },
    "transitive" |: forAll { (a: A, b: A, c: A) =>
      ((a ≲ b) /\ (b ≲ c)) ==> (a ≲ c)
    }
  )

trait Equivalence[A : Arbitrary] extends Preorder[A]:
  def symmetric(a: A, b: A): Prop = (a ≲ b) ⟶ (b ≲ a)

  val equivalenceProps = all(
    preorderProps,
    "symmetric" |: forAll { (a: A, b: A) =>
      (a ≲ b) ==> (b ≲ a)
    }
  )

trait PartialOrder[A : Arbitrary] extends Preorder[A]:
  extension (a: A)
    def ≤(b: A): Boolean = (a ≲ b)
    def ≥(b: A): Boolean = (a ≳ b)

  val partialOrderProps = all(
    preorderProps,
    "antisymmetric" |: forAll { (a: A, b: A) =>
      (a ≅ b) ==> (a == b)
    }
  )

trait TotalPreorder[A : Arbitrary] extends Preorder[A]:
  val totalPreorderProps = all(
    preorderProps,
    "total" |: forAll { (a: A, b: A) =>
      (a ≲ b) \/ (b ≲ a)
    }
  )

trait TotalOrder[A] extends TotalPreorder[A], PartialOrder[A]

trait StrictOrder[A] extends Endorelation[A]:
  extension (a: A)
    def ≨(b: A): Boolean = relates(a, b)
    def ≩(b: A): Boolean = relates(b, a)

  def irreflexive(a: A): Prop = !(a ≨ a)
  def asymmetric(a: A, b: A): Prop = !((a ≨ b) ∧ (b ≨ a))

// PER, functions, partial functions, etc.?
// define instances for common examples and constructions

given intTotalOrder: TotalOrder[Int] with
  override def relates(m: Int, n: Int): Boolean = m <= n

given intStrictOrder: StrictOrder[Int] with
  override def relates(m: Int, n: Int): Boolean = m < n

given stringTotalOrder: TotalOrder[String] with
  override def relates(s: String, t: String): Boolean = s <= t
