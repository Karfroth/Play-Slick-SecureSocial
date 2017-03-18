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
import org.joda.time.DateTime
import com.github.tototoshi.slick.MySQLJodaSupport._

class MyUserService @Inject()(userAccess: UserDataAccess)(protected val dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) extends UserService[BasicUser] with HasDatabaseConfigProvider[JdbcProfile] {
  val logger = Logger("application.controllers.InMemoryUserService")

  def find(providerId: String, userId: String): Future[Option[BasicProfile]] = {
    val u = userAccess.profiles.filter{ p => 
      p.providerId === providerId && p.userId === userId
    }
    
    db.run(u.take(1).result).map(_.headOption).map( uo => uo.map(u => u.toBasicProfile))
  }

  def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {

    val u = userAccess.profiles.filter{ p => 
        p.providerId === providerId && p.email === Option(email)
    }
    
    db.run(u.take(1).result).map(_.headOption).map( uo => uo.map(u => u.toBasicProfile))
  }
  
  private def basicProfileToProfile(bp: BasicProfile): Profile = {
    Profile(
      None,
      bp.providerId,
      bp.userId,
      bp.firstName,
      bp.lastName,
      bp.fullName,
      bp.email,
      bp.avatarUrl,
      bp.authMethod,
      bp.oAuth1Info,
      bp.oAuth2Info,
      bp.passwordInfo
    )
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
      updating <- updateQuery
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
            (userAccess.profiles returning userAccess.profiles.map(_.id)) += basicProfileToProfile(profile)
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
      val session = (for {
        mainProfile <- userAccess.profiles.filter{ p =>
          p.providerId === current.main.providerId && p.userId === current.main.userId
        }.result.map(_.head)
        uId <- userAccess.users.filter(_.mainId === mainProfile.id).result.map(_.head.id)
        toProfileId <- 
          (userAccess.profiles returning userAccess.profiles.map(_.id)) += basicProfileToProfile(to)
        usersToProfilesAdd <- userAccess.usersToProfiles += UserToProfile(0, uId, toProfileId)
      } yield())
      Future.successful(updatedUser)
    }
  }

  def saveToken(token: MailToken): Future[MailToken] = {
    db.run(userAccess.mailTokens += token)
    Future.successful(token)
  }

  def findToken(token: String): Future[Option[MailToken]] = {
    db.run{
      userAccess.mailTokens.filter(_.uuid === token).result.map(_.headOption)
    }
  }

  def deleteToken(uuid: String): Future[Option[MailToken]] = {
    db.run{
      userAccess.mailTokens.filter(_.uuid === uuid).result.map(_.headOption).map{ o =>
        o match {
          case Some(token) =>
            userAccess.mailTokens.filter(_.uuid === uuid).delete
            Some(token)
          case None => None
        }
      }
    }
  }

  def deleteExpiredTokens() {
    userAccess.mailTokens.filter(_.expirationTime < DateTime.now()).delete
  }

  override def updatePasswordInfo(user: BasicUser, info: PasswordInfo): Future[Option[BasicProfile]] = {
    val profileWithPassword = 
      user.identities.find(_.providerId == UsernamePasswordProvider.UsernamePassword)
    val updater = for { 
      foundProfile <- 
        userAccess.profiles if foundProfile.userId === profileWithPassword.get.userId && foundProfile.providerId === UsernamePasswordProvider.UsernamePassword
    } yield (foundProfile.passwordHasher,
              foundProfile.passwordPassword,
              foundProfile.passwordSalt)
              
    val session = for {
      update <- updater.update(Option(info.hasher), Option(info.password), info.salt)
      profile <- userAccess.profiles.filter{ p => 
        p.userId === profileWithPassword.get.userId && p.providerId === UsernamePasswordProvider.UsernamePassword
      }.result.map(_.headOption).map(op => op.map(p => p.toBasicProfile))
    } yield (profile)
    
    db.run(session)

  }

  override def passwordInfoFor(user: BasicUser): Future[Option[PasswordInfo]] = {
    val innerJoin = for {
      (utp, p) <- userAccess.usersToProfiles.join(userAccess.profiles).on(_.profileId === _.id)
    } yield (utp, p)
    
    val profileSession = (for {
      mainProfile <- userAccess.profiles.filter(p => p.userId === user.main.userId && p.providerId === user.main.providerId).result.map(_.head)
      uId <- userAccess.users.filter(u => u.mainId === mainProfile.id).result.map(_.head)
      target <- innerJoin.filter(j => j._1.userId === uId.id && j._2.providerId === UsernamePasswordProvider.UsernamePassword)
                .result.map(_.headOption).map(op => op.map(_._2))
    } yield (target))
    
    val profileWithPassword = db.run(profileSession)
    profileWithPassword.map { optionProfile =>
      optionProfile.flatMap(_.password)
    }
  }
}
