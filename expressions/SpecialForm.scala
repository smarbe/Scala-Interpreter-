package expressions

import values._

trait SpecialForm extends Expression{
	def execute(env: Environment): Value
}