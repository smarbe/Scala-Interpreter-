package expressions

import values._
import ui._

case class Lambda(parameters: List[Identifier], body: Expression) extends SpecialForm {
	def execute(env: Environment): Value = {
	 new Closure(parameters, body, env) 
	}
}