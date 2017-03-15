package models

import securesocial.core.PasswordInfo

import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._
/**
 * @author Joseph Dessens
 * @since 2014-09-01
 */
case class Password(id: Option[Long] = None, hasher: String, password: String, salt: Option[String] = None) {
    def passwordInfo: PasswordInfo = {
        PasswordInfo(hasher, password, salt)
    }
}

class Passwords(tag: Tag) extends Table[Password](tag, "password") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def hasher = column[String]("hasher")
  def password = column[String]("password")
  def salt = column[Option[String]]("salt")

  def * = (id.?, hasher, password, salt) <> (Password.tupled, Password.unapply)
}