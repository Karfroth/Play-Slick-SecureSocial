package models

import securesocial.core.OAuth2Info

import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._
/**
 * @author Joseph Dessens
 * @since 2014-09-01
 */
case class OAuth2(id: Option[Long] = None, accessToken: String, tokenType: Option[String] = None,
                           expiresIn: Option[Int] = None, refreshToken: Option[String] = None) {
    def oAuth2Info: OAuth2Info = {
        OAuth2Info(accessToken, tokenType, expiresIn, refreshToken)
    }
}

class OAuth2s(tag: Tag) extends Table[OAuth2](tag, "oauth2") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def accessToken = column[String]("access_token")
  def tokenType = column[Option[String]]("token_type")
  def expiresIn = column[Option[Int]]("expires_in")
  def refreshToken = column[Option[String]]("refresh_token")

  def * = (id.?, accessToken, tokenType, expiresIn, refreshToken) <> (OAuth2.tupled, OAuth2.unapply)
}