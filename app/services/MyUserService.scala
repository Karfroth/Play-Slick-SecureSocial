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
             target.avatarUrl,
             target.authMethod,
             target.oAuth1Id,
             target.oAuth2Id,
             target.passwordId)
             
    val target = Await.result(db.run(updater.result), Duration.Inf).head
    
    if (!profile.oAuth1Info.isEmpty) {
      (for {
        o1 <- userAccess.oauth1s if o1.id === target._9
      } yield (o1.token, o1.secret)).update(
          profile.oAuth1Info.get.token, profile.oAuth1Info.get.secret)
    }
    
    if (!profile.oAuth2Info.isEmpty) {
      (for {
        o2 <- userAccess.oauth2s if o2.id === target._10
      } yield (o2.accessToken, o2.tokenType, o2.expiresIn, o2.refreshToken)).update(
          profile.oAuth2Info.get.accessToken, profile.oAuth2Info.get.tokenType, 
          profile.oAuth2Info.get.expiresIn, profile.oAuth2Info.get.refreshToken)
    }
    
    if (!profile.passwordInfo.isEmpty) {
      (for {
        p <- userAccess.passwords if p.id === target._11
      } yield (p.hasher, p.password, p.salt)).update(
          profile.passwordInfo.get.hasher, profile.passwordInfo.get.password, 
          profile.passwordInfo.get.salt)
    }
    
    val q = updater.update(profile.providerId,
                           profile.userId,
                           profile.firstName,
                           profile.lastName,
                           profile.fullName,
                           profile.email,
                           profile.avatarUrl,
                           profile.authMethod.method,
                           target._9,
                           target._10,
                           target._11)
                           
    db.run(q)
    
    val user = db.run(userAccess.users.filter( u => u.id === profile.userId ).result)
    user.map(_.head).map(u => getBasicUser(u.id, u.mainId))
    
  }
  
  private def saveNewUser(profile: BasicProfile) = {
    val oauth1id = {
        val idOption = profile.oAuth1Info.map{ o1 =>
          (userAccess.oauth1s returning userAccess.oauth1s.map(_.id)) += OAuth1(None, o1.token, o1.secret)
        }
        idOption.map(v => db.run(v))
      } 
    val oauth2id = {
        val idOption = profile.oAuth2Info.map{ o2 =>
            (userAccess.oauth2s returning userAccess.oauth2s.map(_.id)) += OAuth2(None, o2.accessToken, o2.tokenType, o2.expiresIn, o2.refreshToken)
        }
        idOption.map(v => db.run(v))
    }
    val passwordinfo = {
        val passOption = profile.passwordInfo.map { p =>
          (userAccess.passwords returning userAccess.passwords.map(_.id)) += Password(None, p.hasher, p.password, p.salt)
        }
        passOption.map(v => db.run(v))
    }
    val session = (for {
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
                  oauth1id match {
                    case Some(o1) => Option(Await.result(o1, Duration.Inf))
                    case None => None
                  },
                  oauth2id match {
                    case Some(o2) => Option(Await.result(o2, Duration.Inf))
                    case None => None
                  },
                  passwordinfo match {
                    case Some(p) => Option(Await.result(p, Duration.Inf))
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
        val passwordIdQuery = userAccess.profiles
            .filter(p => p.userId === profile.userId && p.providerId === UsernamePasswordProvider.UsernamePassword)
            
        val passwordId = Await.result(db.run(passwordIdQuery.result).map(_.head.passwordId), Duration.Inf)
        
        userAccess.passwords.filter{ p =>
          p.id === passwordId
        }.update(
            Password(passwordId, 
              profile.passwordInfo.get.hasher, 
              profile.passwordInfo.get.password, 
              profile.passwordInfo.get.salt)
            )
        
        val user = db.run(userAccess.users.filter( u => u.id === profile.userId ).result)
        user.map(_.head).map(u => getBasicUser(u.id, u.mainId))
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

