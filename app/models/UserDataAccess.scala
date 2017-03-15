package models

import javax.inject.{Inject, Singleton}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._

import securesocial.core.{AuthenticationMethod, BasicProfile, OAuth1Info, OAuth2Info, PasswordInfo}

@Singleton()
class UserDataAccess @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
    extends HasDatabaseConfigProvider[JdbcProfile] {
        
        val mailTokens = TableQuery[MailTokens]
        val userAuthenticators = TableQuery[UserAuthenticators]
        val users = TableQuery[Users]
        val oauth1s = TableQuery[OAuth1s]
        val oauth2s = TableQuery[OAuth2s]
        val passwords = TableQuery[Passwords]
        val profiles = TableQuery[Profiles]

    }