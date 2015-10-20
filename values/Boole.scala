package values

import expressions._

class Boole(val defVal: Boolean) extends Literal with Value{
	val value = defVal
	def this(stringVal: String) = {
	  this(stringVal.toBoolean)
	}
	def &&(other: Boole) = this.value && other.defVal 
	def ||(other: Boole) = this.value || other.defVal 
	def !() = new Boole(!this.value)
	override def toString() = this.value.toString
}