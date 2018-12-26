import cats.{ Applicative, Eval, Monad, Traverse }
import cats.kernel.laws._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._

trait SomethingNiceLaws[F[_]] {
  def algebra: SomethingNice[F]
  implicit def M: Monad[F]

  import cats.syntax.apply._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def saveFind(s: Something): IsEq[F[Option[Something]]] = {
    algebra.save(s) >> algebra.findById(s.id) <-> M.pure(Option(s))
  }

  def saveFind2(s: Something): IsEq[F[Option[Something]]] = {
    val prog: F[Option[Something]] = for {
      _ <- algebra.save(s)
      right <- algebra.findById(s.id)
    } yield right

    IsEq(prog, M.pure(Option(s)))
  }

  def findsAreConsistent(s: List[Something]): IsEq[F[Option[Something]]] = {
    val findId: F[Option[Something]] = for {
      _ <- s.traverse { item => algebra.save(item) }
      found <- s match {
        case Nil => M.pure(None)
        case ::(head, _) =>
          algebra.findById(head.id)
      }
    } yield found

    val findName: F[Option[Something]] = for {
      _ <- s.traverse { item => algebra.save(item) }
      found <- s match {
        case Nil => M.pure(List.empty[Something])
        case ::(head, _) =>
          algebra.findByName(head.name)
      }
    } yield found.find(e => s.headOption.exists(_.id == e.id))

    IsEq(findId, findName)
  }
}

object SomethingNiceLaws {
  def apply[F[_]](instance: SomethingNice[F])(implicit ev: Monad[F]) =
    new SomethingNiceLaws[F] {
      override val algebra = instance
      override implicit val M: Monad[F] = ev
    }
}

import org.typelevel.discipline.Laws
import cats.kernel.laws.discipline._
import cats.{ Eq, Monad }
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

trait EmailAlgebraTests[F[_]] extends Laws {
  def laws: SomethingNiceLaws[F]

  def algebra(implicit
    arbEmail: Arbitrary[Something],
    eqFOptEmail: Eq[F[Option[Something]]]) =
    new SimpleRuleSet(
      name = "Emails",
      "find and save compose" -> forAll(laws.saveFind _),
      "find and save compose 2" -> forAll(laws.saveFind2(_)),
      "Finds are consistent" -> forAll(laws.findsAreConsistent(_)))
}

object EmailAlgebraTests {

  def apply[F[_]: Monad](instance: SomethingNice[F]) = new EmailAlgebraTests[F] {
    override val laws: SomethingNiceLaws[F] = SomethingNiceLaws(instance)
  }
}

import org.scalacheck.ScalacheckShapeless._
import cats.implicits._

class EmailRepositorySpecs extends FunSuite with Discipline {
  implicit val uuidArb = Arbitrary.arbUuid
  implicit val somethingArb = implicitly[Arbitrary[Something]]
  implicit val eqFoo: Eq[Option[Something]] = Eq.fromUniversalEquals

  checkAll("EmailRepository", EmailAlgebraTests(new SomethingNiceInRealitity(Nil)).algebra)
}