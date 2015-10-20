package values

import expressions._

class Variable(pContent: Value) extends Value{
	var content = pContent
	override def toString() = "Variable(" + content.toString() + ")"
}