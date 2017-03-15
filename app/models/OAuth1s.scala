package models

import securesocial.core.OAuth1Info

import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._
/**
 * @author Joseph Dessens
 * @since 2014-09-01
 */
case class OAuth1(id: Option[Long] = None, token: String, secret: String) {
    def oAuth1Info: OAuth1Info = {
        OAuth1Info(token, secret)
    }
}

class OAuth1s(tag: Tag) extends Table[OAuth1](tag, "oauth1") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def token = column[String]("token")
  def secret = column[String]("secret")

  def * = (id.?, token, secret) <> (OAuth1.tupled, OAuth1.unapply)
}