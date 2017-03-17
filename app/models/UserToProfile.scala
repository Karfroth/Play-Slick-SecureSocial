package models

import slick.driver.JdbcProfile
import slick.driver.MySQLDriver.api._

case class UserToProfile (id: Long, userId: Long, profileId: Long)

class UsersToProfiles(tag: Tag) extends Table[UserToProfile](tag, "user_to_profile") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def userId = column[Long]("user_id")
  def profileId = column[Long]("profile_id")

  def * = (id, userId, profileId) <> (UserToProfile.tupled, UserToProfile.unapply)
}