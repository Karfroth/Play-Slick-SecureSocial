package models

import com.github.tototoshi.slick.MySQLJodaSupport._
import org.joda.time.DateTime
import securesocial.core.providers.MailToken

import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._
/**
 * @author Joseph Dessens
 * @since 2014-08-07
 */
class MailTokens(tag: Tag) extends Table[MailToken](tag, "mail_token") {
  def uuid = column[String]("uuid", O.PrimaryKey)
  def email = column[String]("email")
  def creationTime = column[DateTime]("creation_time")
  def expirationTime = column[DateTime]("expiration_time")
  def isSignUp = column[Boolean]("is_sign_up")

  def * = (uuid, email, creationTime, expirationTime, isSignUp) <> (MailToken.tupled, MailToken.unapply)
}