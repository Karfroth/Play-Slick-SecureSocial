package models

import securesocial.core.{AuthenticationMethod, BasicProfile, UserProfile, OAuth1Info, OAuth2Info, PasswordInfo}

import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._

//http://stackoverflow.com/questions/28305023/custom-mapping-to-nested-case-class-structure-in-slick-more-than-22-columns

case class Profile(id: Option[Long]=None,
                        providerId: String,
                        userId: String,
                        firstName: Option[String] = None,
                        lastName: Option[String] = None,
                        fullName: Option[String] = None,
                        email: Option[String] = None,
                        avatarUrl: Option[String] = None,
                        authMethod: AuthenticationMethod,
                        oAuth1: Option[OAuth1Info] = None,
                        oAuth2: Option[OAuth2Info] = None,
                        password: Option[PasswordInfo] = None) extends UserProfile {
  def toBasicProfile = {
    BasicProfile(
        providerId,
        userId,
        firstName,
        lastName,
        fullName,
        email,
        avatarUrl,
        authMethod,
        oAuth1,
        oAuth2,
        password
        )
  }
    
}

class Profiles(tag: Tag) extends Table[Profile](tag, "profile") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def providerId = column[String]("provider_id")
  def userId = column[String]("user_id")
  def firstName = column[Option[String]]("first_name")
  def lastName = column[Option[String]]("last_name")
  def fullName = column[Option[String]]("full_name")
  def email = column[Option[String]]("email")
  def avatarUrl = column[Option[String]]("avatar_url")
  def authMethod = column[String]("auth_method")
  
  def oAuth1Token = column[Option[String]]("oauth1_token")
  def oAuth1Secret = column[Option[String]]("oauth1_secret")
  
  def oAuth2AccessToken = column[Option[String]]("oauth2_accesstoken")
  def oAuth2TokenType = column[Option[String]]("oauth2_tokentype")
  def oAuth2ExpiresIn = column[Option[Int]]("oauth2_expiresin")
  def oAuth2RefreshToken = column[Option[String]]("oauth2_refreshtoken")
  
  def passwordHasher = column[Option[String]]("password_hasher")
  def passwordPassword = column[Option[String]]("password_password")
  def passwordSalt = column[Option[String]]("password_salt")
  
  private type OAuth1TupleType = (Option[String], Option[String])
  private type OAuth2TupleType = (Option[String], Option[String], Option[Int], Option[String])
  private type PasswordTupleType = (Option[String], Option[String], Option[String])
  private type ProfileTupleType = (Long, String, String, Option[String], Option[String], Option[String], 
      Option[String], Option[String], String, OAuth1TupleType, OAuth2TupleType, PasswordTupleType)
  
  private val profileTuple = (id, providerId, userId, firstName, lastName, fullName,
      email, avatarUrl, authMethod, (oAuth1Token, oAuth1Secret), 
      (oAuth2AccessToken, oAuth2TokenType, oAuth2ExpiresIn, oAuth2RefreshToken),
      (passwordHasher, passwordPassword, passwordSalt)).shaped[ProfileTupleType]
  
  private val toModel: ProfileTupleType => Profile = { profileTuple => 
    Profile(
      id = Option(profileTuple._1),
      providerId = profileTuple._2,
      userId = profileTuple._3,
      firstName = profileTuple._4,
      lastName = profileTuple._5,
      fullName = profileTuple._6,
      email = profileTuple._7,
      avatarUrl = profileTuple._8,
      authMethod = profileTuple._9 match {
        case "oauth1" => AuthenticationMethod.OAuth1
        case "oauth2" => AuthenticationMethod.OAuth2
        case "openId" => AuthenticationMethod.OpenId
        case "userPassword" => AuthenticationMethod.UserPassword
      },
      oAuth1 = profileTuple._10 match {
        case (None, None) => None
        case (t, s) => Option(OAuth1Info(t.get, s.get))
      },
      oAuth2 = profileTuple._11 match {
        case (None, None, None, None) => None
        case (a, t, e, r) => Option(OAuth2Info(a.get, t, e, r))
      }, 
      password = profileTuple._12 match {
        case (None, None, None) => None
        case (h, p, s) => Option(PasswordInfo(h.get, p.get, s))
      }
    )
  }
  
  private val toTuple: Profile => Option[ProfileTupleType] = { profile =>
    Some {
      (profile.id.get, profile.providerId, profile.userId, 
        profile.firstName, profile.lastName, profile.fullName, profile.email,
        profile.avatarUrl, profile.authMethod.method,
        profile.oAuth1 match {
          case None => (None, None)
          case Some(OAuth1Info(t, s)) => (Option(t), Option(s))
        }, 
        profile.oAuth2 match {
          case None => (None, None, None, None)
          case Some(OAuth2Info(a, t, e, r)) => (Option(a), t, e, r)
        },
        profile.password match {
          case None => (None, None, None)
          case Some(PasswordInfo(h, p, s)) => (Option(h), Option(p), s)
        }
        )
    }
  }
  
  def * = profileTuple <> (toModel,toTuple)
}