package expressions

import values._
import ui._

case class Access(obj: Expression, field: Identifier) extends SpecialForm {
	def execute(env: Environment): Value = {
	  if(obj.execute(env).isInstanceOf[Environment]){
	    val tempEnv = obj.execute(env).asInstanceOf[Environment]
	    tempEnv.find(field)
	  }
	  else
	    throw new TypeException("Object must be of type Environment")
	}
}