package values

import expressions._

class Number(pValue: Double) extends Literal with Value {
	val value = pValue 
	def this(value: String) = this(value.toDouble)
	def +(other: Number) = new Number(this.value + other.value)
	def -(other: Number) = new Number(this.value - other.value)
	def /(other: Number) = new Number(this.value / other.value)
	def *(other: Number) = new Number(this.value * other.value)
	def <(other: Number) = new Boole(this.value < other.value)
	def >(other: Number) = new Boole(this.value > other.value)
	def ==(other: Number) = new Boole(this.value == other.value)
	def !=(other: Number) = new Boole(this.value != other.value)
	override def toString() = this.value.toString()
}