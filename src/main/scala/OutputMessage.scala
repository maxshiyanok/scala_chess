package chess


trait OutputMessage {
  def forUser(targetUser: User): Boolean
  def toString: String
}

case class WelcomeUser(user: User) extends OutputMessage {
  override def forUser(targetUser: User): Boolean = targetUser.name == user.name
  override def toString: String                     = s"Have fun!"
}

case class SendToUser(user: User, text: String) extends OutputMessage {
  override def forUser(targetUser: User): Boolean = targetUser.name == user.name
  override def toString: String                     = text
}

case class SendToUsers(users: Set[User], text: String) extends OutputMessage {
  override def forUser(targetUser: User): Boolean = users.contains(targetUser)
  override def toString: String                     = text
}

case object KeepAlive extends OutputMessage {
  override def forUser(targetUser: User) = true
  override def toString: String            = ""
}
