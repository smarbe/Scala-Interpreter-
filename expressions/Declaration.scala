package expressions

import values._

case class Declaration(name: Identifier, body: Expression) extends SpecialForm {
	def execute(env: Environment): Value = {
	  env.put(List(name), List(body.execute(env)))
	  
	  Notification.OK
	}
}