package models

import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._
/**
 * @author Joseph Dessens
 * @since 2014-09-01
 */
case class User(id: String, mainId: Long)

class Users(tag: Tag) extends Table[User](tag, "user") {
  def id = column[String]("id", O.PrimaryKey)
  def mainId = column[Long]("main_id")

  def * = (id, mainId) <> (User.tupled, User.unapply)
}