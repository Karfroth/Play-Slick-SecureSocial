/**
 * Copyright 2012 Jorge Aliss (jaliss at gmail dot com) - twitter: @jaliss
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package service

import play.api.Logger
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import securesocial.core._
import securesocial.core.providers.{ UsernamePasswordProvider, MailToken }
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import securesocial.core.services.{ UserService, SaveMode }
import securesocial.core.{AuthenticationMethod, BasicProfile, OAuth1Info, OAuth2Info, PasswordInfo}

import models._
import javax.inject.{Inject, Singleton}
import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._

/**
 * A Sample In Memory user service in Scala
 *
 * IMPORTANT: This is just a sample and not suitable for a production environment since
 * it stores everything in memory.
 */
class MyUserService @Inject()(userAccess: UserDataAccess)(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) extends UserService[BasicUser] with HasDatabaseConfigProvider[JdbcProfile] {
  val logger = Logger("application.controllers.InMemoryUserService")

  //
  var users = Map[(String, String), BasicUser]()
  //private var identities = Map[String, BasicProfile]()
  private var tokens = Map[String, MailToken]()

  def find(providerId: String, userId: String): Future[Option[BasicProfile]] = {
    val u = userAccess.getUserQueryTuple.filter{
      queryTuple => 
        queryTuple._1.providerId === providerId && queryTuple._1.userId === userId
    }
    
    userAccess.getBasicProfile(u).map(_.headOption)
  }

  def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    if (logger.isDebugEnabled) {
      logger.debug("users = %s".format(users))
    }

    val u = userAccess.getUserQueryTuple.filter{
      queryTuple => 
        queryTuple._1.providerId === providerId && queryTuple._1.email === Option(email)
    }
    
    userAccess.getBasicProfile(u).map(_.headOption)
  }

  // http://grokbase.com/t/gg/scalaquery/158ynfe7z0/insert-or-get-slick-3
  // This has to be optimized.
  private def getBasicUser(id: String, mainId: Long) = {
    val mainFuture = userAccess.getBasicProfile {
        userAccess.getUserQueryTuple.filter(_._1.id === mainId).take(1)
    }
    val identitiesFuture = {
      val q = userAccess.getUserQueryTuple.filter(p => p._1.userId === id)
      db.run(q.result).map(seq => userAccess.tupleToProfile(seq))
    }
    val main = Await.result(mainFuture, Duration.Inf).head
    val identity = Await.result(identitiesFuture, Duration.Inf).toList
    BasicUser(main, identity)
  }
  
  private def findProfile(profile: BasicProfile) = {
    val q = userAccess.getUserQueryTuple.filter{
      p => p._1.userId === profile.userId && p._1.providerId === profile.providerId
    }
    
    userAccess.getBasicProfile(q).map(_.headOption)
  }
  
  private def updateProfile(profile: BasicProfile) = {
    // Need more work
    val updater = for {
      target <- userAccess.profiles if target.userId === profile.userId && target.providerId === profile.providerId
    } yield (target.providerId,
             target.userId,
             target.firstName,
             target.lastName,
             target.fullName,
             target.email,
             target.avatarUrl)
             
    val q = updater.update(profile.providerId,
                           profile.userId,
                           profile.firstName,
                           profile.lastName,
                           profile.fullName,
                           profile.email,
                           profile.avatarUrl)
                           
    db.run(q)
    
    // Working Progress
    
    val user = db.run(userAccess.users.filter( u => u.id === profile.userId ).result)
    user.map(_.head).map(u => getBasicUser(u.id, u.mainId))
    
  }
  
  private def saveNewUser(profile: BasicProfile) = {
    val session = (for {
          oauth1id <- {
            val o1 = profile.oAuth1Info.get
            (userAccess.oauth1s returning userAccess.oauth1s.map(_.id)) += OAuth1(None, o1.token, o1.secret)
          } if (profile.oAuth1Info.isEmpty != true)
          oauth2id <- {
            val o2 = profile.oAuth2Info.get
            (userAccess.oauth2s returning userAccess.oauth2s.map(_.id)) += OAuth2(None, o2.accessToken, o2.tokenType, o2.expiresIn, o2.refreshToken)
          } if profile.oAuth2Info.isEmpty != true
          passwordinfo <- {
            val p = profile.passwordInfo.get
            (userAccess.passwords returning userAccess.passwords.map(_.id)) += Password(None, p.hasher, p.password, p.salt)
          } if profile.passwordInfo.isEmpty != true
          pID <- (userAccess.profiles returning userAccess.profiles.map(_.id)) += Profile(
                  None,
                  profile.providerId,
                  profile.userId,
                  profile.firstName,
                  profile.lastName,
                  profile.fullName,
                  profile.email,
                  profile.avatarUrl,
                  profile.authMethod.method,
                  profile.oAuth1Info match {
                    case Some(_) => Some(oauth1id)
                    case None => None
                  },
                  profile.oAuth2Info match {
                    case Some(_) => Some(oauth2id)
                    case None => None
                  },
                  profile.passwordInfo match {
                    case Some(_) => Some(passwordinfo)
                    case None => None
                  }
              )
          user <- (userAccess.users returning userAccess.users.map(_.id)
                       into ((user,id) => user.copy(id=id))) += User(profile.userId, pID)
        } yield (user)).transactionally
        val newUser = db.run(session)
        newUser.map(u => getBasicUser(u.id, u.mainId))
  }
  
  def save(profile: BasicProfile, mode: SaveMode): Future[BasicUser] = {
    mode match {
      case SaveMode.SignUp =>
        saveNewUser(profile)
      case SaveMode.LoggedIn =>
        // first see if there is a user with this BasicProfile already.
        findProfile(profile).flatMap {p =>
          p match {
            case Some(existingUser) =>
              updateProfile(profile)
            case None =>
              saveNewUser(profile)
          }
        }

      case SaveMode.PasswordChange =>
        findProfile(profile).flatMap { entry => updateProfile(profile) }.getOrElse(
          // this should not happen as the profile will be there
          throw new Exception("missing profile")
        )
    }
  }

  def link(current: BasicUser, to: BasicProfile): Future[BasicUser] = {
    if (current.identities.exists(i => i.providerId == to.providerId && i.userId == to.userId)) {
      Future.successful(current)
    } else {
      val added = to :: current.identities
      val updatedUser = current.copy(identities = added)
      users = users + ((current.main.providerId, current.main.userId) -> updatedUser)
      Future.successful(updatedUser)
    }
  }

  def saveToken(token: MailToken): Future[MailToken] = {
    Future.successful {
      tokens += (token.uuid -> token)
      token
    }
  }

  def findToken(token: String): Future[Option[MailToken]] = {
    Future.successful { tokens.get(token) }
  }

  def deleteToken(uuid: String): Future[Option[MailToken]] = {
    Future.successful {
      tokens.get(uuid) match {
        case Some(token) =>
          tokens -= uuid
          Some(token)
        case None => None
      }
    }
  }

  //  def deleteTokens(): Future {
  //    tokens = Map()
  //  }

  def deleteExpiredTokens() {
    tokens = tokens.filter(!_._2.isExpired)
  }

  override def updatePasswordInfo(user: BasicUser, info: PasswordInfo): Future[Option[BasicProfile]] = {
    Future.successful {
      for (
        found <- users.values.find(_ == user);
        identityWithPasswordInfo <- found.identities.find(_.providerId == UsernamePasswordProvider.UsernamePassword)
      ) yield {
        val idx = found.identities.indexOf(identityWithPasswordInfo)
        val updated = identityWithPasswordInfo.copy(passwordInfo = Some(info))
        val updatedIdentities = found.identities.patch(idx, Seq(updated), 1)
        val updatedEntry = found.copy(identities = updatedIdentities)
        users = users + ((updatedEntry.main.providerId, updatedEntry.main.userId) -> updatedEntry)
        updated
      }
    }
  }

  override def passwordInfoFor(user: BasicUser): Future[Option[PasswordInfo]] = {
    Future.successful {
      for (
        found <- users.values.find(u => u.main.providerId == user.main.providerId && u.main.userId == user.main.userId);
        identityWithPasswordInfo <- found.identities.find(_.providerId == UsernamePasswordProvider.UsernamePassword)
      ) yield {
        identityWithPasswordInfo.passwordInfo.get
      }
    }
  }
}

// a simple User class that can have multiple identities
//case class BasicUser(main: BasicProfile, identities: List[BasicProfile])

