import cats.{ Applicative, Eval, Monad, Traverse }
import cats.kernel.laws._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._

class SomethingSimplerLaws(algebra: SomethingSimpler) {

  def saveFind(s: Something): IsEq[Option[Something]] = {
    algebra.insert(s)
    algebra.findById(s.id) <-> Option(s)
  }
}

import org.typelevel.discipline.Laws
import cats.kernel.laws.discipline._
import cats.{ Eq, Monad }
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait SimpleTests extends Laws {
  def laws: SomethingSimplerLaws

  def algebra(implicit
    arbEmail: Arbitrary[Something],
    eqFOptEmail: Eq[Option[Something]]) =
    new SimpleRuleSet(
      name = "simple",
      "find and stuffs" -> forAll(laws.saveFind _))
}

object SimpleTests {

  def apply(instance: SomethingSimpler) = new SimpleTests {
    override val laws: SomethingSimplerLaws = new SomethingSimplerLaws(instance)
  }
}

import org.scalacheck.ScalacheckShapeless._
import cats.implicits._

class SimpleSpec extends FunSuite with Discipline {
  implicit val uuidArb = Arbitrary.arbUuid
  implicit val somethingArb = implicitly[Arbitrary[Something]]
  implicit val eqFoo: Eq[Option[Something]] = Eq.fromUniversalEquals

  checkAll("Simplestuff", SimpleTests(new SomethingSimplerNaive()).algebra)
}