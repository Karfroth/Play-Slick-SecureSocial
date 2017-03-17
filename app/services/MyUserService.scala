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
    val u = userAccess.profiles.filter{ p => 
      p.providerId === providerId && p.userId === userId
    }
    
    db.run(u.take(1).result).map(_.headOption).map( uo => uo.map(u => u.toBasicProfile))
  }

  def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    if (logger.isDebugEnabled) {
      logger.debug("users = %s".format(users))
    }

    val u = userAccess.profiles.filter{ p => 
        p.providerId === providerId && p.email === Option(email)
    }
    
    db.run(u.take(1).result).map(_.headOption).map( uo => uo.map(u => u.toBasicProfile))
  }

  private def findProfile(profile: BasicProfile, profiderId: String) = {
    val q = userAccess.profiles.filter{
      p => p.userId === profile.userId && p.providerId === profiderId
    }
 
    db.run(q.result).map(_.headOption).map(po => po.map(p => p.toBasicProfile))
  }
  
  private def updateProfile(profile: BasicProfile) = {
    val q = for { 
      p <- userAccess.profiles if p.userId === profile.userId && p.providerId === profile.providerId 
      } yield (
          p.providerId,
          p.userId,
          p.firstName,
          p.lastName,
          p.fullName,
          p.email,
          p.avatarUrl,
          p.authMethod,
          (p.oAuth1Token, p.oAuth1Secret),
          (p.oAuth2AccessToken, p.oAuth2TokenType,
          p.oAuth2ExpiresIn, p.oAuth2RefreshToken),
          (p.passwordHasher, p.passwordPassword, p.passwordSalt)
        )
    val updateQuery = q.update(profile.providerId,
        profile.userId,
        profile.firstName,
        profile.lastName,
        profile.fullName,
        profile.email,
        profile.avatarUrl,
        profile.authMethod.method,
        profile.oAuth1Info match {
          case None => (None, None)
          case Some(OAuth1Info(t, s)) => (Option(t), Option(s))
        },
        profile.oAuth2Info match {
          case None => (None, None, None, None)
          case Some(OAuth2Info(a, t, e, r)) => (Option(a), t, e, r)
        },
        profile.passwordInfo match {
          case None => (None, None, None)
          case Some(PasswordInfo(h, p, s)) => (Option(h), Option(p), s)
        })
    
    val innerJoin = for {
      (utp, p) <- userAccess.usersToProfiles.join(userAccess.profiles).on(_.profileId === _.id)
    } yield (utp, p)
    
    val profileSession = (for {
      pid <- userAccess.profiles.filter{ p => 
          p.userId === profile.userId && p.providerId === profile.providerId
        }.result.map(_.head.id)
      userId <- userAccess.usersToProfiles.filter{ utp => 
          utp.profileId === pid
        }.result.map(_.head.userId)
      mainId <- userAccess.users.filter { u =>
        u.id === userId
      }.result.map(_.head.mainId)
      profiles <- innerJoin.filter(_._1.userId === userId).result.map(o => o.map(_._2))
    } yield (mainId, profiles))  
    
    db.run(profileSession).map { tup =>
      val mainId = tup._1
      val main = tup._2.filter(_.id == mainId).head
      val identities = tup._2.filterNot(_.id != mainId)
      BasicUser(main.toBasicProfile, identities.map(_.toBasicProfile).toList)
    }
  }
  
  private def saveNewUser(profile: BasicProfile) = {
    val session = (for {
          profileId <- 
            (userAccess.profiles returning userAccess.profiles.map(_.id)) += Profile(
                  None,
                  profile.providerId,
                  profile.userId,
                  profile.firstName,
                  profile.lastName,
                  profile.fullName,
                  profile.email,
                  profile.avatarUrl,
                  profile.authMethod,
                  profile.oAuth1Info,
                  profile.oAuth2Info,
                  profile.passwordInfo
              )
          uId <- (userAccess.users returning userAccess.users.map(_.id)) += User(0, profileId)
          uTp <- (userAccess.usersToProfiles returning userAccess.usersToProfiles.map(_.id)
              into ((userToProfile,id) => userToProfile.copy(id=id))) += UserToProfile(0, uId, profileId)
        } yield (uTp)).transactionally
    val newUser = db.run(session)
    val q = newUser.map{ utp => (for {
        identities <- userAccess.profiles.filter(_.id === utp.profileId).result
        user <- userAccess.users.filter(_.id === utp.userId).result
      } yield (identities, user)).transactionally
    }
    q.flatMap{ r => 
        db.run(r).map{tup => 
            val main = tup._1.filter(_.id == tup._2.head.mainId).head
            val identities = tup._1.filterNot(_.id == tup._2.head.mainId)
            
            BasicUser(main.toBasicProfile, identities.map(_.toBasicProfile).toList)
          }
      }
  }
  
  def save(profile: BasicProfile, mode: SaveMode): Future[BasicUser] = {
    mode match {
      case SaveMode.SignUp =>
        saveNewUser(profile)
      case SaveMode.LoggedIn =>
        // first see if there is a user with this BasicProfile already.
        findProfile(profile, profile.providerId).flatMap {p =>
          p match {
            case Some(existingUser) =>
              updateProfile(profile)
            case None =>
              saveNewUser(profile)
          }
        }

      case SaveMode.PasswordChange =>
        findProfile(profile, UsernamePasswordProvider.UsernamePassword).flatMap{ p =>
          p match {
            case Some(existingUser) =>
              updateProfile(profile)
            case None =>
              throw new Exception("No User")
          }
        }
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

