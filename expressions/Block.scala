package expressions

import ui._
import values._

case class Block(body: List[Expression]) extends SpecialForm{
	def execute(env: Environment): Value = {
	  val curEnv = new Environment(env)
	  for(i <- 0 to body.length - 1){
	    body(i).execute(curEnv)
	  }
	  body(body.length - 1).execute(curEnv)
	}
}