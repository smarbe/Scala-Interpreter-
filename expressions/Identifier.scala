package expressions

import values._

case class Identifier(pName: String) extends Expression with Serializable {
	val name = pName
	
	def execute(env: Environment): Value = {
	  env.find(this)	  
	}
	override def toString() = this.name
}