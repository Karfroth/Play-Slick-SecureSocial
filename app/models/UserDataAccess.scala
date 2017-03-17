package models

import javax.inject.{Inject, Singleton}

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

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
        val profiles = TableQuery[Profiles]
        val usersToProfiles = TableQuery[UsersToProfiles]
        
    }