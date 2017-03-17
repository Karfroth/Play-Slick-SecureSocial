package models

import securesocial.core.BasicProfile

case class BasicUser(main: BasicProfile, identities: List[BasicProfile])