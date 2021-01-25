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

trait TransitiveRelation[A] extends Endorelation[A]

object TransitiveRelation:
  def laws[A](using r: TransitiveRelation[A])(using Arbitrary[A]): Prop = all(
    "transitive" |: forAll { (a: A, b: A, c: A) =>
      (r.relates(a, b) /\ r.relates(b, c)) ==> r.relates(a, c)
    }
  )

trait Preorder[A] extends TransitiveRelation[A]:
  extension (a: A)
    def pre_<=(b: A): Boolean = relates(a, b)
    def pre_>=(b: A): Boolean = (b pre_<= a)
    def pre_=~(b: A): Boolean = (a pre_<= b) /\ (b pre_<= a)

object Preorder:
  def laws[A](using r: Preorder[A])(using Arbitrary[A]): Prop = all(
    TransitiveRelation.laws,
    "reflexive" |: forAll { (a: A) => a pre_<= a }
  )

trait PartialEquivalence[A] extends TransitiveRelation[A]:
  extension (a: A)
    def per_==(b: A): Boolean = relates(a, b)

object PartialEquivalence:
  def laws[A](using r: PartialEquivalence[A])(using Arbitrary[A]): Prop = all(
    TransitiveRelation.laws,
    "symmetric" |: forAll { (a: A, b: A) =>
      (a per_== b) ==> (b per_== a)
    }
  )
trait Equivalence[A] extends Preorder[A], PartialEquivalence[A]:
  extension (a: A)
    def eq_==(b: A): Boolean = relates(a, b)

object Equivalence:
  def laws[A](using Equivalence[A])(using Arbitrary[A]): Prop = all(
    Preorder.laws,
    PartialEquivalence.laws
  )

trait PartialOrder[A] extends Preorder[A]:
  extension (a: A)
    def po_<=(b: A): Boolean = (a pre_<= b)
    def po_>=(b: A): Boolean = (a pre_>= b)
    def po_=~(b: A): Boolean = (a pre_=~ b)

object PartialOrder:
  def laws[A](using PartialOrder[A])(using Arbitrary[A]): Prop = all(
    Preorder.laws,
    "antisymmetric" |: forAll { (a: A, b: A) =>
      (a po_=~ b) ==> (a == b)
    }
  )

trait TotalPreorder[A] extends Preorder[A]:
  extension (a: A)
    def tp_<=(b: A): Boolean = (a pre_<= b)
    def tp_>=(b: A): Boolean = (a pre_>= b)
    def tp_=~(b: A): Boolean = (a pre_=~ b)
    def tp_<(b: A): Boolean = !(b pre_<= a)
    def tp_>(b: A): Boolean = !(a pre_<= b)

object TotalPreorder:
  def laws[A](using TotalPreorder[A])(using Arbitrary[A]): Prop = all(
    Preorder.laws,
    "connex" |: forAll { (a: A, b: A) =>
      (a tp_<= b) \/ (b tp_<= a)
    }
  )

trait TotalOrder[A] extends TotalPreorder[A], PartialOrder[A]:
  extension (a: A)
    def to_<=(b: A): Boolean = (a tp_<= b)
    def to_>=(b: A): Boolean = (a tp_>= b)
    def to_=~(b: A): Boolean = (a tp_=~ b)
    def to_<(b: A): Boolean = (a tp_< b)
    def to_>(b: A): Boolean = (a tp_< b)

object TotalOrder:
  def laws[A](using TotalOrder[A])(using Arbitrary[A]): Prop = all(
    TotalPreorder.laws,
    PartialOrder.laws
  )

trait StrictPartialOrder[A] extends TransitiveRelation[A]:
  extension (a: A)
    def spo_<(b: A): Boolean = relates(a, b)
    def spo_>(b: A): Boolean = (b spo_< a)

object StrictPartialOrder:
  def laws[A](using StrictPartialOrder[A])(using Arbitrary[A]): Prop = all(
    TransitiveRelation.laws,
    "irreflexive" |: forAll { (a: A) => !(a spo_< a) },
    "asymmetric" |: forAll { (a: A, b: A) =>
      !((a spo_< b) /\ (b spo_< a))
    }
  )

trait StrictWeakOrder[A] extends StrictPartialOrder[A]:
  extension (a: A)
    def swo_<(b: A): Boolean = (a spo_< b)
    def swo_>(b: A): Boolean = (a spo_> b)
    def swo_#(b: A): Boolean = !((a spo_< b) \/ (b spo_< a)) // incomparable

object StrictWeakOrder:
  def laws[A](using StrictWeakOrder[A])(using Arbitrary[A]): Prop = all(
    StrictPartialOrder.laws,
    "transitive incomparability" |: forAll { (a: A, b: A, c: A) =>
      ((a swo_# b) /\ (b swo_# c)) ==> (a swo_# c)
    }
  )

// functions, partial functions, etc.?
// define more instances for common examples and constructions

given boolTotalOrder: TotalOrder[Boolean] with
  override def relates(p: Boolean, q: Boolean): Boolean = p <= q

given intTotalOrder: TotalOrder[Int] with
  override def relates(m: Int, n: Int): Boolean = m <= n

given intStrictWeakOrder: StrictWeakOrder[Int] with
  override def relates(m: Int, n: Int): Boolean = m < n

given stringTotalOrder: TotalOrder[String] with
  override def relates(s: String, t: String): Boolean = s <= t

object doubleTotalOrder extends TotalOrder[Double]:
  override def relates(x: Double, y: Double): Boolean = x <= y

case class IntDiv(val value: Int) extends AnyVal

object IntDiv:
  given Arbitrary[IntDiv] =
    Arbitrary(for n <- Arbitrary.arbitrary[Int] yield IntDiv(n))

  given Preorder[IntDiv] with
    override def relates(m: IntDiv, n: IntDiv): Boolean =
      if m.value == 0 then n.value == 0 else (n.value % m.value) == 0

given pairPreorder[A, B](using Preorder[A], Preorder[B]): Preorder[(A, B)] with
  override def relates(p1: (A, B), p2: (A, B)): Boolean =
    (p1._1 pre_<= p2._1) /\ (p1._2 pre_<= p2._2)

// The following functions demonstrate the equivalence between partial orders
// and strict partial orders:
def po2spo[A](po: PartialOrder[A]): StrictPartialOrder[A] = new StrictPartialOrder[A]:
  given PartialOrder[A] = po
  override def relates(a: A, b: A): Boolean = (a po_<= b) /\ (a != b)

def spo2po[A](spo: StrictPartialOrder[A]): PartialOrder[A] = new PartialOrder[A]:
  given StrictPartialOrder[A] = spo
  override def relates(a: A, b: A): Boolean = (a spo_< b) \/ (a == b)

// The following functions demonstrate the equivalence between total preorders
// and strict weak orders:
def tp2swo[A](tp: TotalPreorder[A]): StrictWeakOrder[A] = new StrictWeakOrder[A]:
  given TotalPreorder[A] = tp
  override def relates(a: A, b: A): Boolean = !(b tp_<= a)

def swo2tp[A](swo: StrictWeakOrder[A]): TotalPreorder[A] = new TotalPreorder[A]:
  given StrictWeakOrder[A] = swo
  override def relates(a: A, b: A): Boolean = !(b swo_< a)
