package expressions

import values._

case class Assignment(x: Identifier, update: Expression) extends SpecialForm{
	def execute(env: Environment): Value = {
	  if(x.execute(env).isInstanceOf[Variable]){
	   var temp = x.execute(env).asInstanceOf[Variable]
	   temp.content = update.execute(env)
	  }
	  Notification.DONE
	}
}