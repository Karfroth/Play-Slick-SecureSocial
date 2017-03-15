package models

import securesocial.core.BasicProfile

/**
 * @author Joseph Dessens
 * @since 2014-08-03
 */
case class BasicUser(main: BasicProfile, identities: List[BasicProfile])