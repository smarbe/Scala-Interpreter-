package values

class Notification(msg: String) extends Value {
	override def toString = msg
}

object Notification {
  val VAR_UPDATED = new Notification("VARIABLE UPDATED")
  val BIND_CREATED = new Notification("BINDING CREATED")
  val UNKNOWN = new Notification("UNKNOWN")
  val OK = new Notification("OK")
  val DONE = new Notification("DONE")
  val ERROR = new Notification("ERROR")
}