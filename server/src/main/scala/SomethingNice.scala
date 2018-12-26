import java.util.UUID

import cats.Id

import scala.collection.mutable.ListBuffer

case class Something(id: UUID, name: String, age: Int, likesIceCream: Boolean)

trait SomethingNice[F[_]] {
  def save(s: Something): F[Option[Something]]
  def findById(id: UUID): F[Option[Something]]
  def findByName(name: String): F[List[Something]]
  def exists(id: UUID): F[Boolean]
  def delete(s: Something): F[Option[Something]]
}

class SomethingNiceInRealitity(initialElements: List[Something] = Nil) extends SomethingNice[Id] {
  val list = new ListBuffer[Something]() ++ initialElements

  override def save(s: Something): Id[Option[Something]] = {
    list.append(s)
    Option(s)
  }

  override def findById(id: UUID): Id[Option[Something]] = {
    list.find(_.id == id)
  }

  override def findByName(name: String): Id[List[Something]] = {
    list.filter(_.name == name).toList
  }

  override def exists(id: UUID): Id[Boolean] = {
    list.exists(_.id == id)
  }

  override def delete(s: Something): Id[Option[Something]] = {
    val index = list.indexOf(s)
    if (index > -1) {
      list.remove(index)
      Option(s)
    } else {
      None
    }
  }
}

trait SomethingSimpler {
  def insert(s: Something): Boolean
  def findById(id: UUID): Option[Something]
  def findByName(name: String): List[Something]
}

class SomethingSimplerNaive extends SomethingSimpler {
  val list = new ListBuffer[Something]()
  override def insert(s: Something): Boolean = {
    list.append(s)
    true
  }

  override def findById(id: UUID): Option[Something] = {
    list.find(_.id == id)
  }

  override def findByName(name: String): List[Something] = {
    list.filterNot(_.name == name).toList
  }
}