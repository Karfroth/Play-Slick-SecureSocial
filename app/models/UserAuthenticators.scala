package models

import com.github.tototoshi.slick.MySQLJodaSupport._
import org.joda.time.DateTime

//import slick.ast.ColumnOption.DBType
import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._

/**
 * @author Joseph Dessens
 * @since 2014-09-01
 */
case class UserAuthenticator(id: String, userId: String, expirationDate: DateTime, lastUsed: DateTime, creationDate: DateTime)

class UserAuthenticators(tag: Tag) extends Table[UserAuthenticator](tag, "authenticator") {
  def id = column[String]("id", O.SqlType("varchar(2000)"), O.PrimaryKey)
  def userId = column[String]("user_id")
  def expirationDate = column[DateTime]("expiration_date")
  def lastUsed = column[DateTime]("last_used")
  def creationDate = column[DateTime]("creation_date")

  def * = (id, userId, expirationDate, lastUsed, creationDate) <> (UserAuthenticator.tupled, UserAuthenticator.unapply)
}