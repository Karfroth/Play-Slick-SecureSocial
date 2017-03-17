package models

import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._

case class User(id: Long, mainId: Long)

class Users(tag: Tag) extends Table[User](tag, "user") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def mainId = column[Long]("main_id")

  def * = (id, mainId) <> (User.tupled, User.unapply)
}